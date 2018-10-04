#!/usr/bin/env python
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

def usage():
    sys.stderr.write("usage: %s [dirs | files] [-- <readelf options>]\n"
                        % sys.argv[0])
    sys.exit(1)

(odir, sym_args) = (set(), "")

for i in range(1, len(sys.argv)):
    f = sys.argv[i]
    if f == "--": # sym_args
        sym_args = " ".join(sys.argv[i + 1:])
        break
    if not os.path.exists(f):
        sys.stderr.write("Error: No such file or directory '%s'\n" % f)
        usage()
    else:
        odir.add(f)

def get_symbols(file):
    syms = {}
    for l in os.popen("readelf -W -s %s %s | c++filt" % (sym_args, file)).readlines():
        l = l.strip()
        if not (len(l) and l[0].isdigit() and len(l.split()) == 8):
            continue
        num, value, size, typ, bind, vis, ndx, name = l.split()
        if typ == 'SECTION' or typ == 'FILE': continue
        # I don't think we have many aliases in gcc, re-instate the addr
        # lut otherwise.
        if vis != "DEFAULT": continue
        #value = int(value, 16)
        #size = int(size, 16) if size.startswith('0x') else int(size)
        defined = ndx != "UND"
        globl = bind == "GLOBAL"
        # c++ RID_FUNCTION_NAME dance. FORNOW: Handled as local use
        # Is that correct?
        if name.endswith("::__FUNCTION__") and typ == "OBJECT":
            name = name[0:(len(name) - len("::__FUNCTION__"))]
            if defined: defined = False
        if defined and not globl: continue
        syms.setdefault(name, {})
        syms[name][["use","def"][defined]] = True
        syms[name][["local","global"][globl]] = True
    # Note: we could filter out e.g. debug_* symbols by looking for
    # value in the debug_macro sections.
    return syms

(oprog, nprog) = ({}, {})

def walker(paths):
    prog = {}
    for path in paths:
        if os.path.isdir(path):
            for r, dirs, files in os.walk(path):
                for f in files:
                    # TODO: maybe extract .a to a tmpdir and walk that, too
                    # maybe /there/foolib.a(file.o) as name?
                    if not f.endswith(".o"): continue
                    p = os.path.join(r, f)
                    prog[os.path.normpath(p)] = get_symbols(p)
                for d in dirs:
                    tem = prog.copy()
                    tem.update(walker([os.path.join(r, d)]))
                    prog = tem
        else:
            prog[os.path.normpath(path)] = get_symbols(path)
    return prog

def resolve(prog):
    x = prog.keys()
    use = set()
    # for each unique pair of different files
    for (f, g) in ((f,g) for f in x for g in x if f != g):
        refs = set()
        # for each defined symbol
        for s in (s for s in prog[f] if prog[f][s].get("def") and s in prog[g]):
            if prog[g][s].get("use"):
                refs.add(s)
        for s in refs:
            # Prune externally referenced symbols as speed optimization only
            for i in (i for i in x if s in prog[i]): del prog[i][s]
        use |= refs
    return use

oprog = walker(odir)
oused = resolve(oprog)
for (i,s) in ((i,s) for i in oprog.keys() for s in oprog[i] if oprog[i][s]):
    if oprog[i][s].get("def") and not oprog[i][s].get("use"):
        print("%s: Symbol '%s' declared extern but never referenced externally"
            % (i,s))


