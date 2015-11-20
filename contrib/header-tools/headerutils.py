#! /usr/bin/python2
import os.path
import sys
import shlex
import re
import subprocess
import shutil
import pickle

import multiprocessing 

def find_pound_include (line, use_outside, use_slash):
  inc = re.findall (ur"^\s*#\s*include\s*\"(.+?)\"", line)
  if len(inc) == 1:
    nm = inc[0]
    if use_outside or os.path.exists (nm):
      if use_slash or '/' not in nm:
        return nm
  return ""

def find_system_include (line):
  inc = re.findall (ur"^\s*#\s*include\s*<(.+?)>", line)
  if len(inc) == 1:
    return inc[0]
  return ""
  
def find_pound_define (line):
  inc = re.findall (ur"^\s*#\s*define ([A-Za-z0-9_]+)", line)
  if len(inc) != 0:
    if len(inc) > 1:
      print "What? more than 1 match in #define??"
      print inc
      sys.exit(5)
    return inc[0];
  return ""

def is_pound_if (line):
  inc = re.findall ("^\s*#\s*if\s", line)
  if not inc:
    inc = re.findall ("^\s*#\s*if[n]?def\s", line)
  if inc:
    return True
  return False

def is_pound_endif (line):
  inc = re.findall ("^\s*#\s*endif", line)
  if inc:
    return True
  return False

def find_pound_if (line):
  inc = re.findall (ur"^\s*#\s*if\s+(.*)", line)
  if len(inc) == 0:
    inc = re.findall (ur"^\s*#\s*elif\s+(.*)", line)
  if len(inc) > 0:
    inc2 = re.findall (ur"defined\s*\((.+?)\)", inc[0])
    inc3 = re.findall (ur"defined\s+([a-zA-Z0-9_]+)", inc[0])
    for yy in inc3:
      inc2.append (yy)
    return inc2
  else:
    inc = re.findall (ur"^\s*#\s*ifdef\s(.*)", line)
    if len(inc) == 0:
      inc = re.findall (ur"^\s*#\s*ifndef\s(.*)", line)
    if len(inc) > 0:
      inc2 = re.findall ("[A-Za-z_][A-Za-z_0-9]*", inc[0])
      return inc2
  if len(inc) == 0:
    return list ()
  print "WTF. more than one line returned for find_pound_if"
  print inc
  sys.exit(5)


# IINFO - this is a vector of include information. It consists of 7 elements.
# [0] - base name of the file
# [1] - path leading to this file.
# [2] - orderd list of all headers directly included by this file.
# [3] - Ordered list of any headers included within condionally compiled code.
#       headers files are expected to have all includes one level deep due to
#       the omnipresent guards at the top of the file.  
# [4] - List of all macros which are consumed (used) within this file.
# [5] - list of all macros which may be defined in this file.
# [6] - The source code for this file, if cached.
# [7] - line number info for any headers in the source file.  Indexed by base
#       name, returning the line the include is on.

empty_iinfo =  ("", "", list(), list(), list(), list(), list())

# This function will process a file and extract interesting information.
# DO_MACROS indicates whether macros defined and used should be recorded.
# KEEP_SRC indicates the source for the file should be cached.
def process_include_info (filen, do_macros, keep_src):
  header = False
  if not os.path.exists (filen):
    return empty_iinfo

  sfile = open (filen, "r");
  data = sfile.readlines()
  sfile.close()

  # Ignore the initial #ifdef HEADER_H in header files
  if filen[-2:] == ".h":
    nest = -1
    header = True
  else:
    nest = 0

  macout = list ()
  macin = list()
  incl = list()
  cond_incl = list()
  src_line = { }
  guard = ""

  for line in (data):
    if is_pound_if (line):
      nest += 1
    elif is_pound_endif (line):
      nest -= 1

    nm = find_pound_include (line, True, True)
    if nm != "" and nm not in incl and nm[-2:] == ".h":
      incl.append (nm)
      if nest > 0:
        cond_incl.append (nm)
      if keep_src:
        src_line[nm] = line
      continue

    if do_macros:
      d = find_pound_define (line)
      if d:
        if d not in macout:
          macout.append (d);
          continue

      d = find_pound_if (line)
      if d:
        # The first #if in a header file should be the guard
        if header and len (d) == 1 and guard == "":
          if d[0][-2:] == "_H":
            guard = d
          else:
            guard = "Guess there was no guard..."
        else:
          for mac in d:
            if mac != "defined" and mac not in macin:
              macin.append (mac);

  if not keep_src:
    data = list()

  return (os.path.basename (filen), os.path.dirname (filen), incl, cond_incl,
          macin, macout, data, src_line)

# Extract header info, but no macros or source code.
def process_ii (filen):
  return process_include_info (filen, False, False)

# Extract header information, and collect macro information.
def process_ii_macro (filen):
  return process_include_info (filen, True, False)

# Extract header information, cache the source lines.
def process_ii_src (filen):
  return process_include_info (filen, False, True)

# Extract header information, coolewc macro info and cache the source lines.
def process_ii_macro_src (filen):
  return process_include_info (filen, True, True)


def ii_base (iinfo):
  return iinfo[0]

def ii_path (iinfo):
  return iinfo[1]

def ii_include_list (iinfo):
  return iinfo[2]

def ii_include_list_cond (iinfo):
  return iinfo[3]

def ii_include_list_non_cond (iinfo):
  l = ii_include_list (iinfo)
  for n in ii_include_list_cond (iinfo):
    l.remove (n)
  return l

def ii_macro_consume (iinfo):
  return iinfo[4]
  
def ii_macro_define (iinfo):
  return iinfo[5]

def ii_src (iinfo):
  return iinfo[6]

def ii_src_line (iinfo):
  return iinfo[7]

def ii_read (fname):
  f = open (fname, 'rb')
  incl = pickle.load (f)
  consumes = pickle.load (f)
  defines = pickle.load (f)
  obj = (fname,fname,incl,list(), list(), consumes, defines, list(), list())
  return obj

def ii_write (fname, obj):
  f = open (fname, 'wb')
  pickle.dump (obj[2], f)
  pickle.dump (obj[4], f)
  pickle.dump (obj[5], f)
  f.close ()

# execute a system command which returns file names
def execute_command (command):
  files = list()
  f = os.popen (command)
  for x in f:
    if x[0:2] == "./":
      fn = x.rstrip()[2:]
    else:
      fn = x.rstrip()
    files.append(fn)
  return files

# Try to locate a build directory from PATH
def find_gcc_bld_dir (path):
  blddir = ""
  # Look for blddir/gcc/tm.h
  command = "find " + path + " -mindepth 2 -maxdepth 3 -name tm.h"
  files = execute_command (command)
  for y in files:
    p = os.path.dirname (y)
    if os.path.basename (p) == "gcc":
      blddir = p
      break
  # If not found, try looking a bit deeper
  # Dont look this deep initially because a lot of cross target builds may show
  # up in the list before a native build... but those are better than nothing.
  if not blddir:
    command = "find " + path + " -mindepth 3 -maxdepth 5 -name tm.h"
    files = execute_command (command)
    for y in files:
      p = os.path.dirname (y)
      if os.path.basename (p) == "gcc":
	blddir = p
	break

  return blddir


# Find files matching pattern NAME, return in a list.
# CURRENT is True if you want to include the current directory
# DEEPER is True if you want to search 3 levels below the current directory
# any files with testsuite diurectories are ignored

def find_gcc_files (name, current, deeper):
  files = list()
  command = ""
  if current:
    if not deeper:
      command = "find -maxdepth 1 -name " + name + " -not -path \"./testsuite/*\""
    else:
      command = "find -maxdepth 4 -name " + name + " -not -path \"./testsuite/*\""
  else:
    if deeper:
      command = "find -maxdepth 4 -mindepth 2 -name " + name + " -not -path \"./testsuite/*\""

  if command != "":
    files = execute_command (command)

  return files

# find the list of unique include names found in a file.
def find_unique_include_list_src (data):
  found = list ()
  for line in data:
    d = find_pound_include (line, True, True)
    if d and d not in found and d[-2:] == ".h":
      found.append (d)
  return found

# find the list of unique include names found in a file.
def find_unique_include_list (filen):
  data = open (filen).read().splitlines()
  return find_unique_include_list_src (data)


# Create the macin, macout, and incl vectors for a file FILEN.
# macin are the macros that are used in #if* conditional expressions
# macout are the macros which are #defined
# incl is the list of incluide files encountered
# returned as a tuple of the filename followed by the triplet of lists
# (filen, macin, macout, incl)

def create_macro_in_out (filen):
  sfile = open (filen, "r");
  data = sfile.readlines()
  sfile.close()

  macout = list ()
  macin = list()
  incl = list()

  for line in (data):
    d = find_pound_define (line)
    if d != "":
      if d not in macout:
        macout.append (d);
      continue

    d = find_pound_if (line)
    if len(d) != 0:
      for mac in d:
        if mac != "defined" and mac not in macin:
          macin.append (mac);
      continue

    nm = find_pound_include (line, True, True)
    if nm != "" and nm not in incl:
      incl.append (nm)

  return (filen, macin, macout, incl)

# create the macro information for filen, and create .macin, .macout, and .incl
# files.  Return the created macro tuple.
def create_include_data_files (filen):

  macros = create_macro_in_out (filen)
  depends = macros[1]
  defines = macros[2]
  incls = macros[3]
  
  disp_message = filen
  if len (defines) > 0:
    disp_message = disp_message + " " + str(len (defines)) + " #defines"
  dfile = open (filen + ".macout", "w")
  for x in defines:
    dfile.write (x + "\n")
  dfile.close ()

  if len (depends) > 0:
    disp_message = disp_message + " " + str(len (depends)) + " #if dependencies"
  dfile = open (filen + ".macin", "w")
  for x in depends:
    dfile.write (x + "\n")
  dfile.close ()

  if len (incls) > 0:
    disp_message = disp_message + " " + str(len (incls)) + " #includes"
  dfile = open (filen + ".incl", "w")
  for x in incls:
    dfile.write (x + "\n")
  dfile.close ()

  return macros



# extract data for include file name_h and enter it into the dictionary.
# this does not change once read in.  use_requires is True if you want to 
# prime the values with already created .requires and .provides files.
def get_include_data (name_h, use_requires):
  macin = list()
  macout = list()
  incl = list ()
  if use_requires and os.path.exists (name_h + ".requires"):
    macin = open (name_h + ".requires").read().splitlines()
  elif os.path.exists (name_h + ".macin"):
    macin = open (name_h + ".macin").read().splitlines()

  if use_requires and os.path.exists (name_h + ".provides"):
    macout  = open (name_h + ".provides").read().splitlines()
  elif os.path.exists (name_h + ".macout"):
    macout  = open (name_h + ".macout").read().splitlines()

  if os.path.exists (name_h + ".incl"):
    incl = open (name_h + ".incl").read().splitlines()

  if len(macin) == 0 and len(macout) == 0 and len(incl) == 0:
    return ()
  data = ( name_h, macin, macout, incl )
  return data
  
# find FIND in src, and replace it with the list of headers in REPLACE.
# Remove any duplicates of FIND in REPLACE, and if some of the REPLACE
# headers occur earlier in the include chain, leave them.
# Return the new SRC only if anything changed.
def find_replace_include (find, replace, src):
  res = list()
  seen = { }
  anything = False
  for line in src:
    inc = find_pound_include (line, True, True)
    if inc == find:
      for y in replace:
        if seen.get(y) == None:
          res.append("#include \""+y+"\"\n")
          seen[y] = True
          if y != find:
            anything = True
# if find isnt in the replacement list, then we are deleting FIND, so changes.
      if find not in replace:
        anything = True
    else:
      if inc in replace:
        if seen.get(inc) == None:
          res.append (line)
          seen[inc] = True
      else:
        res.append (line)

  if (anything):
    return res
  else:
    return list()
      

# pass in a require and provide dictionary to be read in.
def read_require_provides (require, provide):
  if not os.path.exists ("require-provide.master"):
    print "require-provide.master file is not available. please run data collection."
    sys.exit(1)
  incl_list = open("require-provide.master").read().splitlines()
  for f in incl_list:
    if os.path.exists (f+".requires"):
      require[os.path.basename (f)] = open (f + ".requires").read().splitlines()
    else:
      require[os.path.basename (f)] = list ()
    if os.path.exists (f+".provides"):
      provide[os.path.basename (f)] = open (f + ".provides").read().splitlines()
    else:
      provide [os.path.basename (f)] = list ()

   
def build_include_list (filen):
  include_files = list()
  sfile = open (filen, "r")
  data = sfile.readlines()
  sfile.close()
  for line in data:
    nm = find_pound_include (line, False, False)
    if nm != "" and nm[-2:] == ".h":
      if nm not in include_files:
        include_files.append(nm)
  return include_files
 
def build_reverse_include_list (filen):
  include_files = list()
  sfile = open (filen, "r")
  data = sfile.readlines()
  sfile.close()
  for line in reversed(data):
    nm = find_pound_include (line, False, False)
    if nm != "":
      if nm not in include_files:
        include_files.append(nm)
  return include_files
     
# Get compilation return code, and compensate for a warning that we want to 
# consider an error when it comes to inlined templates.
def get_make_rc (rc, output):
  rc = rc % 1280
  if rc == 0:
    # This is not considered an error during compilation of an individual file,
    # but it will cause an error during link if it isn't defined.  If this
    # warning is seen during compiling a file, make it a build error so we 
    # don't remove the header.
    h = re.findall ("warning: inline function.*used but never defined", output)
    if len(h) != 0:
      rc = 1
  return rc;

def get_make_output (build_dir, make_opt):
  devnull = open('/dev/null', 'w')
  at_a_time = multiprocessing.cpu_count() * 2
  make = "make -j"+str(at_a_time)+ " "
  if build_dir != "":
    command = "cd " + build_dir +"; " + make + make_opt
  else:
    command = make + make_opt
  process = subprocess.Popen(command, stdout=devnull, stderr=subprocess.PIPE, shell=True)
  output = process.communicate();
  rc = get_make_rc (process.returncode, output[1])
  return (rc , output[1])

def spawn_makes (command_list):
  devnull = open('/dev/null', 'w')
  rc = (0,"", "")
  proc_res = list()
  text = "  Trying target builds : "
  for command_pair in command_list:
    tname = command_pair[0]
    command = command_pair[1]
    text += tname + ", "
    c = subprocess.Popen(command, bufsize=-1, stdout=devnull, stderr=subprocess.PIPE, shell=True)
    proc_res.append ((c, tname))

  print text[:-2]

  for p in proc_res:
    output = p[0].communicate()
    ret = (get_make_rc (p[0].returncode, output[1]), output[1], p[1])
    if (ret[0] != 0):
      # Just record the first one.
      if rc[0] == 0:
        rc = ret;
  return rc

def get_make_output_parallel (targ_list, make_opt, at_a_time):
  command = list()
  targname = list()
  if at_a_time == 0:
    at_a_time = multiprocessing.cpu_count() * 2
  proc_res = [0] * at_a_time
  for x in targ_list:
    if make_opt[-2:] == ".o":
      s = "cd " + x[1] + "/gcc/; make " + make_opt
    else:
      s = "cd " + x[1] +"; make " + make_opt
    command.append ((x[0],s))

  num = len(command) 
  rc = (0,"", "")
  loops = num // at_a_time
  
  if (loops > 0):
    for idx in range (loops):
      ret = spawn_makes (command[idx*at_a_time:(idx+1)*at_a_time])
      if ret[0] != 0:
        rc = ret
        break

  if (rc[0] == 0):
    leftover = num % at_a_time
    if (leftover > 0):
      ret = spawn_makes (command[-leftover:])
      if ret[0] != 0:
        rc = ret

  return rc


def readwholefile (src_file):
  sfile = open (src_file, "r")
  src_data = sfile.readlines()
  sfile.close()
  return src_data

