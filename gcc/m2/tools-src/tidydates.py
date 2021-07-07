#!/usr/bin/python3

# utility to tidy dates and detect lack of copyright.

# Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

import os, sys

maxLineLength = 60


#
#  visitDir - call func for each file below, dir, matching extension, ext.
#

def visitDir (dir, ext, func):
    listOfFiles = os.listdir(dir)
    listOfFiles.sort()
    for file in listOfFiles:
        if os.path.isfile(os.path.join(dir, file)):
            l = len(ext)
            if (len(file)>l) and (file[-l:] == ext):
                func(os.path.join(dir, file))
        elif os.path.isdir(os.path.join(dir, file)):
            visitDir(os.path.join(dir, file), ext, func)

#
#  isYear - returns True if, year, is legal.
#

def isYear (year):
    if len(year)==5:
        year = year[:-1]
    for c in year:
        if not c.isdigit():
            return False
    return True


#
#  handleCopyright -
#

def handleCopyright (outfile, lines, n, leader1, leader2):
    global maxLineLength
    i = lines[n]
    c = i.find('Copyright (C) ')+len('Copyright (C)')
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
            if len(d)==1:
                d = []
            else:
                d = d[1:]

            if c>maxLineLength:
                outfile.write('\n')
                outfile.write(leader1)
                outfile.write(leader2)
                outfile.write(' '*(start-2))
                c = start

            if isYear(e):
                if (e[-1]=='.') or (e[-1]==','):
                    punctuation = e[-1]
                    e = e[:-1]
                else:
                    punctuation = ""
            else:
                seenDate = False
            if seenDate:
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

#
#  handleHeader - reads in the header of a file and inserts
#                 a line break around the Copyright dates.
#

def handleHeader (file, leader1, leader2):
    print("------------------------------")
    l = open(file, 'r').readlines()
    if len(l)>20:
        outfile = open('tmptidy', 'w')
        n = 0
        for i in l:
            if i.find('Copyright (C)')>=0:
                outfile, n = handleCopyright(outfile, l, n, leader1, leader2)
                outfile.writelines(l[n:])
                outfile.close()
                print("-> mv tmptidy", file)
                command = "mv tmptidy %s" % file
                os.system(command)
                return
            else:
                outfile.write(l[n])
                n += 1
        outfile.close()
        sys.stdout.write("%s:1:1 needs a Copyright notice..\n" % file)


#
#  bashTidy - tidy up dates using '#' comment
#

def bashTidy (file):
    handleHeader(file, '#', ' ')

#
#  cTidy - tidy up dates using '/* */' comments
#

def cTidy (file):
    handleHeader(file, ' ', '*')

#
#  m2Tidy - tidy up dates using '(* *)' comments
#

def m2Tidy (file):
    handleHeader(file, ' ', ' ')

#
#  main - for each file extension call the appropriate tidy
#         routine.
#

def main ():
    visitDir('.', '.in', bashTidy)
    visitDir('.', '.py', bashTidy)
    visitDir('.', '.c', cTidy)
    visitDir('.', '.h', cTidy)
    visitDir('.', '.def', m2Tidy)
    visitDir('.', '.mod', m2Tidy)


main ()
