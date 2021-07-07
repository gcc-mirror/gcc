#!/usr/bin/env python3
# 
# boilerplate.py utility to rewrite the boilerplate with new dates.
# 
# Copyright (C) 2018-2021 Free Software Foundation, Inc.
# Contributed by Gaius Mulley <gaius@glam.ac.uk>.
# 
# This file is part of GNU Modula-2.
# 
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# GNU Modula-2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU Modula-2; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.
#
import sys
import os
import glob
import sys, getopt, string
import datetime

forceGPL3x, forceGPL3 = False, False
doModify, verbose = True, False,
multiFilemode, updateAll, forceCheck = False, False, False

summaryGiven, contributedBy, outputName = "", "", "-"
errorCount = 0
startDir = "."
seenFiles = []


#
#  printf - keeps C programmers happy :-)
#

def printf (fmt, *args):
    print(str (fmt) % args, end=' ')

#
#  error - issue an error message.
#

def error (fmt, *args):
    global errorCount

    print(str (fmt) % args, end=' ')
    errorCount += 1


def haltOnError ():
    if errorCount > 0:
        os.sys.exit (1)


def basename (f):
    b = f.split ("/")
    return b[-1]


#
#  analyseComment -
#

def analyseComment (text, f):
    start_date, end_date, contribution, summary, lic = None, None, None, None, None
    if text.find ("Copyright ISO/IEC") > 0:
        lic = "BSISO"
        now = datetime.datetime.now ()
        for d in range (1984, now.year+1):
            if text.find (str (d)) > 0:
                if start_date == None:
                    start_date = str (d)
                end_date = str (d)
        return start_date, end_date, "", "", lic
    elif text.find ("Copyright (C)") > 0:
        if text.find ("GNU General Public License") > 0:
            lic = "GPL"
        elif text.find ("GNU Lesser General") > 0:
            lic = "LGPL"
        if text.find ("version 2.1") > 0:
            lic += "v2.1"
        elif text.find ("version 2") > 0:
            lic += "v2"
        elif text.find ("version 3") > 0:
            lic += "v3"
        if text.find ("GCC Runtime Library Exception") > 0:
            lic += "x"
        now = datetime.datetime.now ()
        for d in range (1984, now.year+1):
            if text.find (str (d)) > 0:
                if start_date == None:
                    start_date = str (d)
                end_date = str (d)
        if text.find ("ontributed by") > 0:
            i = text.find ("ontributed by")
            i += len ("ontributed by")
            j = text.index (". ", i)
            contribution = text[i:j]
    if text.find (basename (f)) > 0:
        i = text.find (basename (f))
        j = text.find (". ", i)
        if j < 0:
            error ('summary of the file does not finish with a "."')
            summary = text[i:]
        else:
            summary = text[i:j]
    return start_date, end_date, contribution, summary, lic


#
#  analyseHeader -
#

def analyseHeader (f, start, end):
    text = ""
    if end == None:
        for count, l in enumerate (open (f, "r").readlines ()):
            parts = l.split (start)
            if len (parts) > 1:
                line = start.join (parts[1:])
                line = line.rstrip ()
                line = line.lstrip ()
                text += " "
                text += line
            elif (l.rstrip () != "") and (len (parts[0]) > 0):
                return analyseComment (text, f), count
    else:
        inComment = False
        for count, l in enumerate (open (f, "r").readlines ()):
            while l != "":
                l = l.strip ()
                l = l.rstrip ()
                if inComment:
                    text += " "
                    pos = l.find (end)
                    if pos >= 0:
                        text += l[:pos]
                        l = l[pos:]
                        inComment = False
                    else:
                        text += l
                        l = ""
                else:
                    pos = l.find (start)
                    if (pos >= 0) and (len (l) > len (start)):
                        before = l[:pos]
                        before = before.rstrip ()
                        before = before.lstrip ()
                        if before != "":
                            return analyseComment (text, f), count
                        l = l[pos + len (start):]
                        inComment = True
                    elif (l != "") and (l == end):
                        l = ""
                    else:
                        return analyseComment (text, f), count
    return [None, None, None, None, None], 0


#
#  addStop - add a full stop to a sentance.
#

def addStop (sentence):
    if sentence is None:
        return None
    sentence = sentence.rstrip ()
    if (len (sentence) > 0) and (sentence[-1] != "."):
        return sentence + "."
    return sentence


GPLv3 = """
%s

Copyright (C) %s Free Software Foundation, Inc.
Contributed by %s

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.
"""

GPLv3x = """
%s

Copyright (C) %s Free Software Foundation, Inc.
Contributed by %s

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.
"""

LGPLv3 = """
%s

Copyright (C) %s Free Software Foundation, Inc.
Contributed by %s

This file is part of GNU Modula-2.

GNU Modula-2 is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with GNU Modula-2.  If not, see <https://www.gnu.org/licenses/>.
"""

BSISO = """
Library module defined by the International Standard
   Information technology - programming languages
   BS ISO/IEC 10514-1:1996E Part 1: Modula-2, Base Language.

   Copyright ISO/IEC (International Organization for Standardization
   and International Electrotechnical Commission) %s.

   It may be freely copied for the purpose of implementation (see page
   707 of the Information technology - Programming languages Part 1:
   Modula-2, Base Language.  BS ISO/IEC 10514-1:1996).
"""

templates = { "GPLv3":GPLv3,
              "GPLv3x":GPLv3x,
              "LGPLv3":LGPLv3,
              "LGPLv2.1":LGPLv3,
              "BSISO":BSISO }


def writeTemplate (fo, magic, start, end, dates, contribution, summary, lic):
    if lic in templates:
        if lic == "BSISO":
            # non gpl but freely distributed for the implementation of a compiler
            text = templates[lic] % (dates)
            text = text.rstrip ()
        else:
            summary = summary.lstrip ()
            contribution = contribution.lstrip ()
            summary = addStop (summary)
            contribution = addStop (contribution)
            if magic != None:
                fo.write (magic)
                fo.write ("\n")
            text = templates[lic] % (summary, dates, contribution)
            text = text.rstrip ()
        if end == None:
            text = text.split ("\n")
            for line in text:
                fo.write (start)
                fo.write (" ")
                fo.write (line)
                fo.write ("\n")
        else:
            text = text.lstrip ()
            fo.write (start)
            fo.write (" ")
            fo.write (text)
            fo.write ("  ")
            fo.write (end)
            fo.write ("\n")
        # add a blank comment line for a script for eye candy.
        if start == "#" and end == None:
            fo.write (start)
            fo.write ("\n")
    else:
        error ("no template found for: %s\n", lic)
        os.sys.exit (1)
    return fo


def writeBoilerPlate (fo, magic, start, end, start_date, end_date, contribution, summary, gpl):
    if start_date == end_date:
        dates = start_date
    else:
        dates = "%s-%s" % (start_date, end_date)
    return writeTemplate (fo, magic, start, end, dates, contribution, summary, gpl)


def rewriteFile (f, magic, start, end, start_date, end_date, contribution, summary, gpl, lines):
    l = open (f, "r").readlines ()[lines:]
    text = "".join (l)
    if outputName == "-":
        fo = sys.stdout
    else:
        fo = open (f, "w")
    fo = writeBoilerPlate (fo, magic, start, end, start_date, end_date, contribution, summary, gpl)
    fo.write (text)
    fo.flush ()
    if outputName != "-":
        fo.close ()


#
#  handleHeader - keep reading lines of file, f, looking for start, end
#                 sequences and comments inside.  The comments are checked
#                 for:  date, contribution, summary
#

def handleHeader (f, magic, start, end):
    global date, contribution, summary, doModify, forceCheck, errorCount

    errorCount = 0
    [start_date, end_date, contribution, summary, lic], lines =  analyseHeader (f, start, end)
    if lic == None:
        error ("%s:1:no GPL found at the top of the file\n", f)
    else:
        if verbose:
            printf ("copyright: %s\n", lic)
            if (start_date != None) and (end_date != None):
                if start_date == end_date:
                    printf ("dates = %s\n", start_date)
                else:
                    printf ("dates = %s-%s\n", start_date, end_date)
            if summary != None:
                printf ("summary: %s\n", summary)
            if contribution != None:
                printf ("contribution: %s\n", contribution)
        if start_date == None:
            error ("%s:1:no date found in the GPL at the top of the file\n", f)
        if contribution == None:
            if contributedBy == "":
                error ("%s:1:no contribution found in the GPL at the top of the file\n", f)
            else:
                contribution = contributedBy
        if summary == None:
            if summaryGiven == "":
                error ("%s:1:no single line summary found in the GPL at the top of the file\n", f)
            else:
                summary = summaryGiven
    if errorCount == 0:
        now = datetime.datetime.now ()
        if doModify:
            if lic == "BSISO":
                # don't change the BS ISO license!
                pass
            elif forceGPL3x:
                lic = "GPLv3x"
            elif forceGPL3:
                lic = "GPLv3"
            rewriteFile (f, magic, start, end, start_date, str (now.year), contribution, summary, lic, lines)
        elif forceCheck:
            print(f, "suppressing change as requested", start_date, end_date, lic)
    else:
        printf ("too many errors, no modifications will occur\n")


#
#  bashTidy - tidy up dates using '#' comment
#

def bashTidy (f):
    handleHeader (f, "#!/bin/bash", "#", None)


#
#  pythonTidy - tidy up dates using '#' comment
#

def pythonTidy (f):
    handleHeader (f, "#!/usr/bin/env python3", '#', None)


#
#  bnfTidy - tidy up dates using '--' comment
#

def bnfTidy (f):
    handleHeader (f, None, '--', None)


#
#  cTidy - tidy up dates using '/* */' comments
#

def cTidy (f):
    handleHeader (f, None, '/*', '*/')

#
#  m2Tidy - tidy up dates using '(* *)' comments
#

def m2Tidy (f):
    handleHeader (f, None, '(*', '*)')

#
#  inTidy - tidy up dates using '#' as a comment and check the first line for magic number.
#

def inTidy (f):
    first = open (f, "r").readlines ()[0]
    if (len (first) > 0) and (first[:2] == "#!"):
        # magic number found, use this
        handleHeader (f, first, "#", None)
    else:
        handleHeader (f, None, "#", None)


#
#  doVisit -
#

def doVisit (args, dirname, names):
    global outputName
    func, extension = args
    for f in names:
        if len (f) > len (extension) and f[-len (extension):] == extension:
            # print os.path.join (dirname, f)
            outputName = f
            func (os.path.join (dirname, f))


#
#  visitDir - visit
#

def visitDir (startDir, extension, func):
    global outputName, seenFiles
    # os.walk (startDir, doVisit, [func, extension])
    for dirName, subdirList, fileList in os.walk(startDir):
        for fname in fileList:
            if (len (fname) > len (extension)) and (fname[-len(extension):] == extension):
                fullpath = os.path.join (dirName, fname)
                outputName = fullpath
                # printf ("outputName = %s\n", outputName)
                if not (fullpath in seenFiles):
                    seenFiles += [fullpath]
                    func (fullpath)
            # Remove the first entry in the list of sub-directories
            # if there are any sub-directories present
        if len(subdirList) > 0:
            del subdirList[0]

#
#  findFiles - for each file extension call the appropriate tidy
#              routine.
#

def findFiles ():
    visitDir (startDir, '.h.in', cTidy)
    visitDir (startDir, '.in', inTidy)
    visitDir (startDir, '.sh', inTidy)
    visitDir (startDir, '.py', pythonTidy)
    visitDir (startDir, '.c', cTidy)
    visitDir (startDir, '.h', cTidy)
    visitDir (startDir, '.cc', cTidy)
    visitDir (startDir, '.def', m2Tidy)
    visitDir (startDir, '.mod', m2Tidy)
    visitDir (startDir, '.bnf', bnfTidy)


#
#  usage - output very brief usage instructions.
#

def usage (code = 0):
    print("boilerplate [-c contributionstring] [ -s summarystring ] [-d] [-v] [-g] [-x] [-o outputfile] inputfile.c")
    print("  -o outputfile   (this must be before the final inputfile on the command line).")
    print("  -c              a string which will be used as the contribution line.")
    print("  -s              a string which will be used as the summary line.")
    print("  -f              force a check to insist that the contribution, summary and GPL exists.")
    print("  -g              change to GPLv3.")
    print("  -x              change to GPLv3 with GCC runtime extension.")
    print("  -r directory    recusively scan directory for known file extensions (.def, .mod, .c, .h, .py, .in, .sh).")
    print("  -u              update all dates.")
    print("  -v              verbose.")
    print("  -N              do not modify any file")
    os.sys.exit (code)


#
#  handleArguments - check the legal arguments.
#

def handleArguments ():
    global multiFilemode, contributedBy, updateAll, forceCheck, outputName, verbose, startDir, doModify, forceGPL3, forceGPL3x, summaryGiven
    try:
        optlist, l = getopt.getopt (sys.argv[1:],':c:dfgho:r:s:uvxN')
    except getopt.GetoptError:
        usage (1)
    for opt in optlist:
        if opt[0] == '-c':
            contributedBy = opt[1]
        if opt[0] == '-s':
            summaryGiven = opt[1]
        if opt[0] == '-d':
            debugging = True
        if opt[0] == '-f':
            forceCheck = True
        if opt[0] == '-g':
            forceGPL3 = True
        if opt[0] == '-x':
            forceGPL3x = True
        if opt[0] == '-h':
            usage ()
        if opt[0] == '-r':
            multiFilemode = True
            startDir = opt[1]
        if opt[0] == '-o':
            outputName = opt[1]
        if opt[0] == '-u':
            updateAll = True
        if opt[0] == '-v':
            verbose = True
        if opt[0] == '-N':
            doModify = False
    if l == []:
        return None
    return l[0]


#
#  hasExt - return True if, name, ends with, ext.
#

def hasExt (name, ext):
    if len (name) > len (ext):
        return name[-len (ext):] == ext
    return False


#
#  singleFile - scan the single file for a GPL boilerplate which
#               has a GPL, contribution field and a summary heading.
#

def singleFile (i):
    if hasExt (i, ".def") or hasExt (i, ".mod"):
        m2Tidy (i)
    elif hasExt (i, ".h") or hasExt (i, ".c") or hasExt (i, ".cc"):
        cTidy (i)
    elif hasExt (i, ".in"):
        inTidy (i)
    elif hasExt (i, ".sh"):
        inTidy (i)  # uses magic number for actual sh/bash
    elif hasExt (i, ".py"):
        pythonTidy (i)


#
#  main - handleArguments and then find source files.
#

def main ():
    i = handleArguments ()
    if multiFilemode:
        findFiles ()
    elif i == None:
        print("an input file must be specified on the command line")
        usage (1)
    else:
        singleFile (i)
    haltOnError ()


main ()
