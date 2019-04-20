#!/usr/bin/env python3
#
# Check gcc.pot file for gcc-internal-format and print all strings
# that contain an option that is not wrapped by %<-option_name%>.
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

import argparse
import re

parser = argparse.ArgumentParser(description='')
parser.add_argument('file', help = 'pot file')

args = parser.parse_args()

origin = None
internal = False

lines = open(args.file).readlines()
for i, l in enumerate(lines):
    l = l.strip()
    s = 'msgid '
    if l.startswith('#: '):
        origin = l
    elif '#, gcc-internal-format' in l:
        internal = True
    if l.startswith(s) and origin and internal:
        j = 0
        while not lines[i + j].startswith('msgstr'):
            l = lines[i + j]
            if l.startswith(s):
                l = l[len(s):]
            text = l.strip('"').strip()
            if text:
                parts = text.split(' ')
                for p in parts:
                    if p.startswith('-'):
                        if len(p) >= 2 and (p[1].isalpha() and p != '-INF'):
                            print('%s: %s' % (origin, text))
                    elif p.startswith('__builtin_'):
                        print('%s: %s' % (origin, text))
                    if re.search("[^%]'", p):
                        print('%s: %s' % (origin, text))
                    # %< should not be preceded by a non-punctuation
                    # %character.
                    if re.search("[a-zA-Z0-9]%<", p):
                        print('%s: %s' % (origin, text))
            j += 1

        origin = None
        internal = False
