#!/usr/bin/env python3

# Copyright (C) 2021 Free Software Foundation, Inc.
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

import sys
import os
import glob
import string
import getopt

outputFile = "-"
inputFile = "-"
subDirectory = None
webRelease = "12"


def usage (code):
    print ("usage:  createhtml [-d] [-h] [-o outputfile] [-s subdirectory] [-w webrelease]")
    os.sys.exit (code)


def collectArgs ():
    global outputFile, subDirectory, webrelease
    try:
        optlist, arglist = getopt.getopt(sys.argv[1:],':dho:s:vw:')
    except getopt.GetoptError:
        usage (1)
    for opt in optlist:
        if opt[0] == '-d':
            debugging = True
        if opt[0] == '-h':
            usage (0)
        if opt[0] == '-v':
            verbose = True
        if opt[0] == '-s':
            subDirectory = opt[1]
        if opt[0] == '-o':
            outputFile = opt[1]
        if opt[0] == '-w':
            webRelease = opt[1]
    return arglist


#  transformHtml - copy inputfile to outputFile replacing
#  HOME_HREF and 12_HREF with html tags.

def transformHtml ():
    if inputFile == "-":
        usage (1)
    inputFileFd = open (inputFile, "r")
    if outputFile == "-":
        outputFileFd = sys.stdout
    else:
        outputFileFd = open (outputFile, "w")
    text = inputFileFd.read ()
    if subDirectory is None or subDirectory == ".":
        text = text.replace ("HOME_HREF", '<a href="homepage.html">Home</a>')
        text = text.replace ("11_HREF", '<a href="11/homepage.html">GCC-11</a>')
        text = text.replace ("12_HREF", '<a href="12/homepage.html">GCC-12</a>')
        text = text.replace ("__WEBRELEASE__", webRelease)
        text = text.replace ("__RELEASE__", ".")
    else:
        text = text.replace ("HOME_HREF", '<a href="../homepage.html">Home</a>')
        text = text.replace ("11_HREF", '<a href="../11/homepage.html">GCC-11</a>')
        text = text.replace ("12_HREF", '<a href="../12/homepage.html">GCC-12</a>')
        text = text.replace ("__WEBRELEASE__", "../" + webRelease)
        text = text.replace ("__RELEASE__", "../" + subDirectory)
    outputFileFd.write (text)
    outputFileFd.close ()



#  main - collect arguments and call transformHtml.

def main ():
    global inputFile
    arglist = collectArgs ()
    if arglist != []:
        inputFile = arglist[0]
    transformHtml ()


main()
