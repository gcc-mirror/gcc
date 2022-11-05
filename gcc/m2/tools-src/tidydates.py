#!/usr/bin/env python3

# utility to tidy dates and detect lack of copyright.

# Copyright (C) 2016-2022 Free Software Foundation, Inc.
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
# along with GNU Modula-2; see the file COPYING.  If not, write to the
# Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import os
import sys
import pathlib
import shutil

maxLineLength = 60

COPYRIGHT = "Copyright (C)"


def visitDir(directory, ext, func):
    # visitDir - call func for each file below, dir, matching extension, ext.
    listOfFiles = os.listdir(directory)
    listOfFiles.sort()
    for filename in listOfFiles:
        path = pathlib.PurePath(filename)
        full = os.path.join(directory, filename)
        if path.is_file(full):
            if path.suffix == ext:
                func(full)
        elif path.is_dir(full):
            visitDir(full, ext, func)


def isYear(year):
    # isYear - returns True if, year, is legal.
    if len(year) == 5:
        year = year[:-1]
    for c in year:
        if not c.isdigit():
            return False
    return True


def handleCopyright(outfile, lines, n, leader1, leader2):
    # handleCopyright look for Copyright in the comment.
    global maxLineLength
    i = lines[n]
    c = i.find(COPYRIGHT)+len(COPYRIGHT)
    outfile.write(i[:c])
    d = i[c:].split()
    start = c
    seenDate = True
    years = []
    while seenDate:
        if d == []:
            n += 1
            i = lines[n]
            d = i[2:].split()
        else:
            e = d[0]
            punctuation = ""
            if len(d) == 1:
                d = []
            else:
                d = d[1:]
            if c > maxLineLength:
                outfile.write("\n")
                outfile.write(leader1)
                outfile.write(leader2)
                outfile.write(" "*(start-2))
                c = start
            if isYear(e):
                if (e[-1] == ".") or (e[-1] == ","):
                    punctuation = e[-1]
                    e = e[:-1]
                else:
                    punctuation = ""
            else:
                seenDate = False
            if seenDate:
                if not (e in years):
                    c += len(e) + len(punctuation)
                    outfile.write(" ")
                    outfile.write(e)
                    outfile.write(punctuation)
                    years += [e]
            else:
                if start < c:
                    outfile.write("\n")
                    outfile.write(leader1)
                    outfile.write(leader2)
                    outfile.write(" "*(start-2))

                outfile.write(" ")
                outfile.write(e)
                outfile.write(punctuation)
                for w in d:
                    outfile.write(" ")
                    outfile.write(w)
    outfile.write("\n")
    return outfile, n+1


def handleHeader(filename, leader1, leader2):
    # handleHeader reads in the header of a file and inserts
    # a line break around the Copyright dates.
    print("------------------------------")
    lines = open(filename, "r").readlines()
    if len(lines) > 20:
        with open("tmptidy", "w") as outfile:
            n = 0
            for i in lines:
                if i.find("Copyright (C)") >= 0:
                    outfile, n = handleCopyright(outfile, lines,
                                                 n, leader1, leader2)
                    outfile.writelines(lines[n:])
                    outfile.close()
                    print("-> mv tmptidy", filename)
                    shutil.move("tmptidy", filename)
                    return
                else:
                    outfile.write(lines[n])
                    n += 1
        sys.stdout.write("%s:1:1 needs a Copyright notice..\n" % filename)


def bashTidy(filename):
    # bashTidy - tidy up dates using "#" comment
    handleHeader(filename, "#", " ")


def cTidy(filename):
    # cTidy - tidy up dates using "/* */" comments
    handleHeader(filename, " ", "*")


def m2Tidy(filename):
    # m2Tidy - tidy up dates using "(* *)" comments
    handleHeader(filename, " ", " ")


def main():
    # main - for each file extension call the appropriate tidy routine.
    visitDir(".", ".in", bashTidy)
    visitDir(".", ".py", bashTidy)
    visitDir(".", ".c", cTidy)
    visitDir(".", ".h", cTidy)
    visitDir(".", ".def", m2Tidy)
    visitDir(".", ".mod", m2Tidy)


main()
