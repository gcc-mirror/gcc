#!/usr/bin/env python3

# Copyright (C) 2018-2025 Free Software Foundation, Inc.
#
# Script to analyze warnings produced by clang.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.
#
#
#

import argparse


def skip_warning(filename, message):
    ignores = {
            '': ['-Warray-bounds', '-Wmismatched-tags',
                 'gcc_gfc: -Wignored-attributes', '-Wchar-subscripts',
                 'string literal (potentially insecure): -Wformat-security',
                 '-Wdeprecated-register',
                 '-Wvarargs', 'keyword is hidden by macro definition',
                 "but the argument has type 'char *': -Wformat-pedantic",
                 '-Wnested-anon-types',
                 'qualifier in explicit instantiation of',
                 'attribute argument not supported: asm_fprintf',
                 'when in C++ mode, this behavior is deprecated',
                 '-Wignored-attributes', '-Wgnu-zero-variadic-macro-arguments',
                 '-Wformat-security', '-Wundefined-internal',
                 '-Wunknown-warning-option', '-Wc++20-extensions',
                 '-Wbitwise-instead-of-logical', 'egrep is obsolescent'],
            'insn-modes.cc': ['-Wshift-count-overflow'],
            'insn-emit.cc': ['-Wtautological-compare'],
            'insn-attrtab.cc': ['-Wparentheses-equality'],
            'gimple-match.cc': ['-Wunused-', '-Wtautological-compare'],
            'generic-match.cc': ['-Wunused-', '-Wtautological-compare'],
            'i386.md': ['-Wparentheses-equality', '-Wtautological-compare',
                        '-Wtautological-overlap-compare'],
            'sse.md': ['-Wparentheses-equality', '-Wtautological-compare',
                       '-Wconstant-logical-operand'],
            'mmx.md': ['-Wtautological-compare'],
            'genautomata.cc': ['-Wstring-plus-int'],
            'fold-const-call.cc': ['-Wreturn-type'],
            'gfortran.texi': [''],
            'libtool': [''],
            'lex.cc': ['-Wc++20-attribute-extensions'],
    }

    for name, ignore in ignores.items():
        for i in ignore:
            if name in filename and i in message:
                return True
    return False


parser = argparse.ArgumentParser()
parser.add_argument('log', help='Log file with clang warnings')
args = parser.parse_args()

lines = [line.strip() for line in open(args.log)]
messages = set()
for line in lines:
    token = ': warning: '
    i = line.find(token)
    if i != -1:
        location = line[:i]
        message = line[i + len(token):]
        if '/libffi/' in location or location.startswith('Makefile'):
            continue
        if not skip_warning(location, message):
            messages.add(line)

for line in sorted(messages):
    print(line)

print('\nTotal warnings: %d' % len(messages))
