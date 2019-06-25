#!/usr/bin/env python3
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
# <http://www.gnu.org/licenses/>.  */
#
#
#

import sys
import argparse

def skip_warning(filename, message):
    ignores = {
            '': ['-Warray-bounds', '-Wmismatched-tags', 'gcc_gfc: -Wignored-attributes', '-Wchar-subscripts',
                'string literal (potentially insecure): -Wformat-security', '-Wdeprecated-register',
                '-Wvarargs', 'keyword is hidden by macro definition', "but the argument has type 'char *': -Wformat-pedantic",
                '-Wnested-anon-types', 'qualifier in explicit instantiation of', 'attribute argument not supported: asm_fprintf',
                'when in C++ mode, this behavior is deprecated', '-Wignored-attributes', '-Wgnu-zero-variadic-macro-arguments',
                '-Wformat-security'],
            'insn-modes.c': ['-Wshift-count-overflow'],
            'insn-emit.c': ['-Wtautological-compare'],
            'insn-attrtab.c': ['-Wparentheses-equality'],
            'gimple-match.c': ['-Wunused-', '-Wtautological-compare'],
            'generic-match.c': ['-Wunused-', '-Wtautological-compare'],
            'i386.md': ['-Wparentheses-equality', '-Wtautological-compare'],
            'sse.md': ['-Wparentheses-equality', '-Wtautological-compare'],
            'genautomata.c': ['-Wstring-plus-int']

    }

    for name, ignores in ignores.items():
        for i in ignores:
            if name in filename and i in message:
                return True

    return False

parser = argparse.ArgumentParser()
parser.add_argument('log', help = 'Log file with clang warnings')
args = parser.parse_args()

lines = [l.strip() for l in open(args.log)]
total = 0
messages = []
for l in lines:
    token = ': warning: '
    i = l.find(token)
    if i != -1:
        location = l[:i]
        message = l[i + len(token):]
        if not skip_warning(location, message):
            total += 1
            messages.append(l)

for l in sorted(messages):
    print(l)
print('\nTotal warnings: %d' % total)
