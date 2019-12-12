#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright (c) 2018 Free Software Foundation
# Contributed by Bernhard Reutner-Fischer <aldot@gcc.gnu.org>
# Inspired by bloat-o-meter from busybox.

# This software may be used and distributed according to the terms and
# conditions of the GNU General Public License as published by the Free
# Software Foundation.

# For a set of object-files, determine symbols that are
#  - public but should be static

# Examples:
# unused_functions.py ./gcc/fortran
# unused_functions.py gcc/c  gcc/c-family/ gcc/*-c.o | grep -v "'gt_"
# unused_functions.py gcc/cp gcc/c-family/ gcc/*-c.o | grep -v "'gt_"

import sys, os
from tempfile import mkdtemp
from subprocess import Popen, PIPE

def usage():
    sys.stderr.write("usage: %s [-v] [dirs | files] [-- <readelf options>]\n"
                        % sys.argv[0])
    sys.stderr.write("\t-v\tVerbose output\n");
    sys.exit(1)

(odir, sym_args, tmpd, verbose) = (set(), "", None, False)

for i in range(1, len(sys.argv)):
    f = sys.argv[i]
    if f == '--': # sym_args
        sym_args = ' '.join(sys.argv[i + 1:])
        break
    if f == '-v':
        verbose = True
        continue
    if not os.path.exists(f):
        sys.stderr.write("Error: No such file or directory '%s'\n" % f)
        usage()
    else:
        if f.endswith('.a') and tmpd is None:
            tmpd = mkdtemp(prefix='unused_fun')
        odir.add(f)

def dbg(args):
    if not verbose: return
    print(args)

def get_symbols(file):
    syms = {}
    rargs = "readelf -W -s %s %s" % (sym_args, file)
    p0 = Popen((a for a in rargs.split(' ') if a.strip() != ''), stdout=PIPE)
    p1 = Popen(["c++filt"], stdin=p0.stdout, stdout=PIPE,
            universal_newlines=True)
    lines = p1.communicate()[0]
    for l in lines.split('\n'):
        l = l.strip()
        if not len(l) or not l[0].isdigit(): continue
        larr = l.split()
        if len(larr) != 8: continue
        num, value, size, typ, bind, vis, ndx, name = larr
        if typ == 'SECTION' or typ == 'FILE': continue
        # I don't think we have many aliases in gcc, re-instate the addr
        # lut otherwise.
        if vis != 'DEFAULT': continue
        #value = int(value, 16)
        #size = int(size, 16) if size.startswith('0x') else int(size)
        defined = ndx != 'UND'
        globl = bind == 'GLOBAL'
        # c++ RID_FUNCTION_NAME dance. FORNOW: Handled as local use
        # Is that correct?
        if name.endswith('::__FUNCTION__') and typ == 'OBJECT':
            name = name[0:(len(name) - len('::__FUNCTION__'))]
            if defined: defined = False
        if defined and not globl: continue
        syms.setdefault(name, {})
        syms[name][['use','def'][defined]] = True
        syms[name][['local','global'][globl]] = True
    # Note: we could filter out e.g. debug_* symbols by looking for
    # value in the debug_macro sections.
    if p1.returncode != 0:
        print("Warning: Reading file '%s' exited with %r|%r"
            % (file, p0.returncode, p1.returncode))
    p0.kill()
    return syms

(oprog, nprog) = ({}, {})

def walker(paths):
    def ar_x(archive):
        dbg("Archive %s" % path)
        f = os.path.abspath(archive)
        f = os.path.splitdrive(f)[1]
        d = tmpd + os.path.sep + f
        d = os.path.normpath(d)
        owd = os.getcwd()
        try:
            os.makedirs(d)
            os.chdir(d)
            p0 = Popen(["ar", "x", "%s" % os.path.join(owd, archive)],
                    stderr=PIPE, universal_newlines=True)
            p0.communicate()
            if p0.returncode > 0: d = None # assume thin archive
        except:
            dbg("ar x: Error: %s: %s" % (archive, sys.exc_info()[0]))
            os.chdir(owd)
            raise
        os.chdir(owd)
        if d: dbg("Extracted to %s" % (d))
        return (archive, d)

    def ar_t(archive):
        dbg("Thin archive, using existing files:")
        try:
            p0 = Popen(["ar", "t", "%s" % archive], stdout=PIPE,
                    universal_newlines=True)
            ret = p0.communicate()[0]
            return ret.split('\n')
        except:
            dbg("ar t: Error: %s: %s" % (archive, sys.exc_info()[0]))
            raise

    prog = {}
    for path in paths:
        if os.path.isdir(path):
            for r, dirs, files in os.walk(path):
                if files: dbg("Files %s" % ", ".join(files))
                if dirs: dbg("Dirs  %s" % ", ".join(dirs))
                prog.update(walker([os.path.join(r, f) for f in files]))
                prog.update(walker([os.path.join(r, d) for d in dirs]))
        else:
            if path.endswith('.a'):
                if ar_x(path)[1] is not None: continue # extract worked
                prog.update(walker(ar_t(path)))
            if not path.endswith('.o'): continue
            dbg("Reading symbols from %s" % (path))
            prog[os.path.normpath(path)] = get_symbols(path)
    return prog

def resolve(prog):
    x = prog.keys()
    use = set()
    # for each unique pair of different files
    for (f, g) in ((f,g) for f in x for g in x if f != g):
        refs = set()
        # for each defined symbol
        for s in (s for s in prog[f] if prog[f][s].get('def') and s in prog[g]):
            if prog[g][s].get('use'):
                refs.add(s)
        for s in refs:
            # Prune externally referenced symbols as speed optimization only
            for i in (i for i in x if s in prog[i]): del prog[i][s]
        use |= refs
    return use

try:
    oprog = walker(odir)
    if tmpd is not None:
        oprog.update(walker([tmpd]))
    oused = resolve(oprog)
finally:
    try:
        p0 = Popen(["rm", "-r", "-f", "%s" % (tmpd)], stderr=PIPE, stdout=PIPE)
        p0.communicate()
        if p0.returncode != 0: raise "rm '%s' didn't work out" % (tmpd)
    except:
        from shutil import rmtree
        rmtree(tmpd, ignore_errors=True)

for (i,s) in ((i,s) for i in oprog.keys() for s in oprog[i] if oprog[i][s]):
    if oprog[i][s].get('def') and not oprog[i][s].get('use'):
        print("%s: Symbol '%s' declared extern but never referenced externally"
            % (i,s))


