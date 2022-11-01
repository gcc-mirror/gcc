#!/usr/bin/env python3

# def2doc.py creates texi library documentation for all exported procedures.
# Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

# Copyright (C) 2000-2022 Free Software Foundation, Inc.
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
#

import argparse
import os
import sys

BaseLibs = ["gm2-libs", "Base libraries", "Basic M2F compatible libraries"]

PIMLogDesc = "PIM and Logitech 3.0 compatible libraries"
PIMLog = ["gm2-libs-pim", "PIM and Logitech 3.0 Compatible", PIMLogDesc]
PIMCorDesc = "PIM compatible process support"
PIMCor = ["gm2-libs-coroutines", "PIM coroutine support", PIMCorDesc]
ISOLibs = ["gm2-libs-iso", "M2 ISO Libraries", "ISO defined libraries"]

libraryClassifications = [BaseLibs, PIMLog, PIMCor, ISOLibs]


def initState():
    global inVar, inType, inConst
    inVar, inType, inConst = False, False, False


def emitNode(name, nxt, previous, up):
    if args.texinfo:
        output.write("@node " + name + ", " + nxt + ", ")
        output.write(previous + ", " + up + "\n")
    elif args.sphinx:
        output.write("@c @node " + name + ", " + nxt + ", ")
        output.write(previous + ", " + up + "\n")


def emitSection(name):
    if args.texinfo:
        output.write("@section " + name + "\n")
    elif args.sphinx:
        output.write(name + "\n")
        output.write("=" * len(name) + "\n")


def emitSubSection(name):
    if args.texinfo:
        output.write("@subsection " + name + "\n")
    elif args.sphinx:
        output.write(name + "\n")
        output.write("-" * len(name) + "\n")


#  displayLibraryClass - displays a node for a library directory and invokes
#                        a routine to summarize each module.

def displayLibraryClass():
    global args
    previous = ""

    nxt = libraryClassifications[1][1]
    i = 0
    lib = libraryClassifications[i]

    while True:
        emitNode(lib[1], nxt, previous, args.up)
        emitSection(lib[1])
        output.write("\n")
        displayModules(lib[1], lib[0], args.builddir, args.sourcedir)
        output.write("\n")
        output.write("@c " + "-" * 60 + "\n")
        previous = lib[1]
        i += 1
        if i == len(libraryClassifications):
            break
        lib = libraryClassifications[i]
        if i+1 == len(libraryClassifications):
            nxt = ""
        else:
            nxt = libraryClassifications[i+1][1]


#  displayMenu - displays the top level menu for library documentation.

def displayMenu():
    output.write("@menu\n")
    for lib in libraryClassifications:
        output.write("* " + lib[1] + "::" + lib[2] + "\n")
    output.write("@end menu\n")

    output.write("\n")
    output.write("@c " + "=" * 60 + "\n")
    output.write("\n")


#  removeInitialComments - removes any (* *) at the top
#                          of the definition module.

def removeInitialComments(file, line):
    while (str.find(line, "*)") == -1):
        line = file.readline()


#  removeableField - returns True if a comment field should be removed
#  from the definition module.

def removeableField(line):
    field_list = ["Author", "Last edit", "LastEdit", "Last update",
                  "Date", "Title", "Revision"]
    for field in field_list:
        if (str.find(line, field) != -1) and (str.find(line, ":") != -1):
            return True
    ignore_list = ["System", "SYSTEM"]
    for ignore_field in ignore_list:
        if str.find(line, ignore_field) != -1:
            if str.find(line, ":") != -1:
                if str.find(line, "Description:") == -1:
                    return True
    return False


#  removeFields - removes Author/Date/Last edit/SYSTEM/Revision
#                 fields from a comment within the start of a
#                 definition module.

def removeFields(file, line):
    while (str.find(line, "*)") == -1):
        if not removeableField(line):
            output.write(str.replace(str.replace(str.rstrip(line),
                                                 "{", "@{"), "}", "@}") + "\n")
        line = file.readline()
    output.write(str.rstrip(line) + "\n")


#  checkIndex - create an index entry for a PROCEDURE, TYPE, CONST or VAR.

def checkIndex(line):
    global inVar, inType, inConst

    words = str.split(line)
    procedure = ""
    if (len(words) > 1) and (words[0] == "PROCEDURE"):
        inConst = False
        inType = False
        inVar = False
        if (words[1] == "__BUILTIN__") and (len(words) > 2):
            procedure = words[2]
        else:
            procedure = words[1]

    if (len(line) > 1) and (line[0:2] == "(*"):
        inConst = False
        inType = False
        inVar = False
    elif line == "VAR":
        inConst = False
        inVar = True
        inType = False
        return
    elif line == "TYPE":
        inConst = False
        inType = True
        inVar = False
        return
    elif line == "CONST":
        inConst = True
        inType = False
        inVar = False
    if inVar:
        words = str.split(line, ",")
        for word in words:
            word = str.lstrip(word)
            if word != "":
                if str.find(word, ":") == -1:
                    output.write("@findex " + word + " (var)\n")
                elif len(word) > 0:
                    var = str.split(word, ":")
                    if len(var) > 0:
                        output.write("@findex " + var[0] + " (var)\n")

    if inType:
        words = str.lstrip(line)
        if str.find(words, "=") != -1:
            word = str.split(words, "=")
            if (len(word[0]) > 0) and (word[0][0] != "_"):
                output.write("@findex " + str.rstrip(word[0]) + " (type)\n")
        else:
            word = str.split(words)
            if (len(word) > 1) and (word[1] == ";"):
                # hidden type
                if (len(word[0]) > 0) and (word[0][0] != "_"):
                    output.write("@findex " + str.rstrip(word[0]))
                    output.write(" (type)\n")

    if inConst:
        words = str.split(line, ";")
        for word in words:
            word = str.lstrip(word)
            if word != "":
                if str.find(word, "=") != -1:
                    var = str.split(word, "=")
                    if len(var) > 0:
                        output.write("@findex " + var[0] + " (const)\n")

    if procedure != "":
        name = str.split(procedure, "(")
        if name[0] != "":
            proc = name[0]
            if proc[-1] == ";":
                proc = proc[:-1]
            if proc != "":
                output.write("@findex " + proc + "\n")


#  parseDefinition - reads a definition module and creates
#                    indices for procedures, constants,
#                    variables and types.

def parseDefinition(dir, source, build, file, needPage):
    output.write("\n")
    with open(findFile(dir, build, source, file), "r") as f:
        initState()
        line = f.readline()
        while (str.find(line, "(*") != -1):
            removeInitialComments(f, line)
            line = f.readline()

        while (str.find(line, "DEFINITION") == -1):
            line = f.readline()

        output.write("@example\n")
        output.write(str.rstrip(line) + "\n")
        line = f.readline()
        if len(str.rstrip(line)) == 0:
            output.write("\n")
            line = f.readline()
            if (str.find(line, "(*") != -1):
                removeFields(f, line)
            else:
                output.write(str.rstrip(line) + "\n")
        else:
            output.write(str.rstrip(line) + "\n")
        line = f.readline()
        while line:
            line = str.rstrip(line)
            checkIndex(line)
            output.write(str.replace(str.replace(line, "{", "@{"), "}", "@}"))
            output.write("\n")
            line = f.readline()
        output.write("@end example\n")
        if needPage:
            output.write("@page\n")


def parseModules(up, dir, build, source, listOfModules):
    previous = ""
    i = 0
    if len(listOfModules) > 1:
        nxt = dir + "/" + listOfModules[1][:-4]
    else:
        nxt = ""
    while i < len(listOfModules):
        emitNode(dir + "/" + listOfModules[i][:-4], nxt, previous, up)
        emitSubSection(dir + "/" + listOfModules[i][:-4])
        parseDefinition(dir, source, build, listOfModules[i], True)
        output.write("\n")
        previous = dir + "/" + listOfModules[i][:-4]
        i = i + 1
        if i+1 < len(listOfModules):
            nxt = dir + "/" + listOfModules[i+1][:-4]
        else:
            nxt = ""


#  doCat - displays the contents of file, name, to stdout

def doCat(name):
    with open(name, "r") as file:
        line = file.readline()
        while line:
            output.write(str.rstrip(line) + "\n")
            line = file.readline()


#  moduleMenu - generates a simple menu for all definition modules
#               in dir

def moduleMenu(dir, build, source):
    output.write("@menu\n")
    listOfFiles = []
    if os.path.exists(os.path.join(source, dir)):
        listOfFiles += os.listdir(os.path.join(source, dir))
    if os.path.exists(os.path.join(source, dir)):
        listOfFiles += os.listdir(os.path.join(build, dir))
    listOfFiles = list(dict.fromkeys(listOfFiles).keys())
    listOfFiles.sort()
    for file in listOfFiles:
        if foundFile(dir, build, source, file):
            if (len(file) > 4) and (file[-4:] == ".def"):
                output.write("* " + dir + "/" + file[:-4] + "::" + file + "\n")
    output.write("@end menu\n")
    output.write("\n")


#  checkDirectory - returns True if dir exists in either build or source.

def checkDirectory(dir, build, source):
    if os.path.isdir(build) and os.path.exists(os.path.join(build, dir)):
        return True
    elif os.path.isdir(source) and os.path.exists(os.path.join(source, dir)):
        return True
    else:
        return False


#  foundFile - return True if file is found in build/dir/file or
#  source/dir/file.

def foundFile(dir, build, source, file):
    name = os.path.join(os.path.join(build, dir), file)
    if os.path.exists(name):
        return True
    name = os.path.join(os.path.join(source, dir), file)
    if os.path.exists(name):
        return True
    return False


#  findFile - return the path to file searching in build/dir/file
#  first then source/dir/file.

def findFile(dir, build, source, file):
    name1 = os.path.join(os.path.join(build, dir), file)
    if os.path.exists(name1):
        return name1
    name2 = os.path.join(os.path.join(source, dir), file)
    if os.path.exists(name2):
        return name2
    sys.stderr.write("file cannot be found in either " + name1)
    sys.stderr.write(" or " + name2 + "\n")
    os.sys.exit(1)


#  displayModules - walks though the files in dir and parses
#                   definition modules and includes README.texi

def displayModules(up, dir, build, source):
    if checkDirectory(dir, build, source):
        if foundFile(dir, build, source, "README.texi"):
            doCat(findFile(dir, build, source, "README.texi"))

        moduleMenu(dir, build, source)
        listOfFiles = []
        if os.path.exists(os.path.join(source, dir)):
            listOfFiles += os.listdir(os.path.join(source, dir))
        if os.path.exists(os.path.join(source, dir)):
            listOfFiles += os.listdir(os.path.join(build, dir))
        listOfFiles = list(dict.fromkeys(listOfFiles).keys())
        listOfFiles.sort()
        listOfModules = []
        for file in listOfFiles:
            if foundFile(dir, build, source, file):
                if (len(file) > 4) and (file[-4:] == ".def"):
                    listOfModules += [file]
        listOfModules.sort()
        parseModules(up, dir, build, source, listOfModules)
    else:
        line = "directory " + dir + " not found in either "
        line += build + " or " + source
        sys.stderr.write(line + "\n")


def displayCopyright():
    output.write("@c Copyright (C) 2000-2022 Free Software Foundation, Inc.\n")
    output.write("@c This file is part of GNU Modula-2.\n")
    output.write("""
@c Permission is granted to copy, distribute and/or modify this document
@c under the terms of the GNU Free Documentation License, Version 1.2 or
@c any later version published by the Free Software Foundation.
""")


def collectArgs():
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", help="generate progress messages",
                        action="store_true")
    parser.add_argument("-b", "--builddir", help="set the build directory",
                        default=".", action="store")
    parser.add_argument("-f", "--inputfile", help="set the input file",
                        default=None, action="store")
    parser.add_argument("-o", "--outputfile", help="set the output file",
                        default=None, action="store")
    parser.add_argument("-s", "--sourcedir", help="set the source directory",
                        default=".", action="store")
    parser.add_argument("-t", "--texinfo",
                        help="generate texinfo documentation",
                        default=False, action="store_true")
    parser.add_argument("-u", "--up", help="set the up node",
                        default="", action="store")
    parser.add_argument("-x", "--sphinx", help="generate sphinx documentation",
                        default=False, action="store_true")
    args = parser.parse_args()
    return args


def handleFile():
    if args.inputfile is None:
        displayCopyright()
        displayMenu()
        displayLibraryClass()
    else:
        parseDefinition(".", args.sourcedir, args.builddir,
                        args.inputfile, False)


def main():
    global args, output
    args = collectArgs()
    if args.outputfile is None:
        output = sys.stdout
        handleFile()
    else:
        with open(args.outputfile, "w") as output:
            handleFile()


main()
