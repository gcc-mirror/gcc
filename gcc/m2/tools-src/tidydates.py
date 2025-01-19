#!/usr/bin/env python3

# utility to tidy dates and detect lack of copyright.

# Copyright (C) 2016-2025 Free Software Foundation, Inc.
#
# This file is part of GNU Modula-2.
#
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Modula-2 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

import os
import pathlib
import shutil
import sys

max_line_length = 60

COPYRIGHT = 'Copyright (C)'


def visit_dir(directory, ext, func):
    # visit_dir - call func for each file below, dir, matching extension, ext.
    list_of_files = os.listdir(directory)
    list_of_files.sort()
    for filename in list_of_files:
        path = pathlib.PurePath(filename)
        full = os.path.join(directory, filename)
        if path.is_file(full):
            if path.suffix == ext:
                func(full)
        elif path.is_dir(full):
            visit_dir(full, ext, func)


def is_year(year):
    # is_year - returns True if, year, is legal.
    if len(year) == 5:
        year = year[:-1]
    for c in year:
        if not c.isdigit():
            return False
    return True


def handle_copyright(outfile, lines, n, leader1, leader2):
    # handle_copyright look for Copyright in the comment.
    global max_line_length
    i = lines[n]
    c = i.find(COPYRIGHT)+len(COPYRIGHT)
    outfile.write(i[:c])
    d = i[c:].split()
    start = c
    seen_date = True
    years = []
    while seen_date:
        if d == []:
            n += 1
            i = lines[n]
            d = i[2:].split()
        else:
            e = d[0]
            punctuation = ''
            if len(d) == 1:
                d = []
            else:
                d = d[1:]
            if c > max_line_length:
                outfile.write('\n')
                outfile.write(leader1)
                outfile.write(leader2)
                outfile.write(' '*(start-2))
                c = start
            if is_year(e):
                if (e[-1] == '.') or (e[-1] == ','):
                    punctuation = e[-1]
                    e = e[:-1]
                else:
                    punctuation = ''
            else:
                seen_date = False
            if seen_date:
                if not (e in years):
                    c += len(e) + len(punctuation)
                    outfile.write(' ')
                    outfile.write(e)
                    outfile.write(punctuation)
                    years += [e]
            else:
                if start < c:
                    outfile.write('\n')
                    outfile.write(leader1)
                    outfile.write(leader2)
                    outfile.write(' '*(start-2))

                outfile.write(' ')
                outfile.write(e)
                outfile.write(punctuation)
                for w in d:
                    outfile.write(' ')
                    outfile.write(w)
    outfile.write('\n')
    return outfile, n+1


def handle_header(filename, leader1, leader2):
    # handle_header reads in the header of a file and inserts
    # a line break around the Copyright dates.
    print('------------------------------')
    lines = open(filename).readlines()
    if len(lines) > 20:
        with open('tmptidy', 'w') as outfile:
            n = 0
            for i in lines:
                if i.find('Copyright (C)') >= 0:
                    outfile, n = handle_copyright(outfile, lines,
                                                  n, leader1, leader2)
                    outfile.writelines(lines[n:])
                    outfile.close()
                    print('-> mv tmptidy', filename)
                    shutil.move('tmptidy', filename)
                    return
                else:
                    outfile.write(lines[n])
                    n += 1
        sys.stdout.write('%s:1:1 needs a Copyright notice..\n' % filename)


def bash_tidy(filename):
    # bash_tidy - tidy up dates using '#' comment
    handle_header(filename, '#', ' ')


def c_tidy(filename):
    # c_tidy - tidy up dates using '/* */' comments
    handle_header(filename, ' ', '*')


def m2_tidy(filename):
    # m2_tidy - tidy up dates using '(* *)' comments
    handle_header(filename, ' ', ' ')


def main():
    # main - for each file extension call the appropriate tidy routine.
    visit_dir('.', '.in', bash_tidy)
    visit_dir('.', '.py', bash_tidy)
    visit_dir('.', '.c', c_tidy)
    visit_dir('.', '.h', c_tidy)
    visit_dir('.', '.def', m2_tidy)
    visit_dir('.', '.mod', m2_tidy)


main()
