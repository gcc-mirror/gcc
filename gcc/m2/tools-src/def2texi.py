#!/usr/bin/python3

# def2texi.py creates texi library documentation for all exported procedures.
# Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

# Copyright (C) 2000-2021 Free Software Foundation, Inc.
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

import sys
import os
import glob
import getopt

libraryClassifications = [['gm2-libs','Base libraries',
                           'Basic M2F compatible libraries'],
                          ['gm2-libs-pim','PIM and Logitech 3.0 Compatible',
                           'PIM and Logitech 3.0 compatible libraries'],
                          ['gm2-libs-coroutines','PIM coroutine support',
                           'PIM compatible process support'],
                          ['gm2-libs-iso','M2 ISO Libraries',
                           'ISO defined libraries']]

def initState ():
    global inVar, inType, inConst
    inVar, inType, inConst = False, False, False


#
#  displayLibraryClass - displays a node for a library directory and invokes
#                        a routine to summarize each module
#

def displayLibraryClass():
    global buildDir, up
    previous = ""

    next=libraryClassifications[1][1]
    i = 0
    l = libraryClassifications[i]

    while True:
        print("@node " + l[1] + ", " + next + ", " + previous + ", " + up)
        print("@section " + l[1])
        print("")
        displayModules(l[1], l[0], buildDir, sourceDir)
        print("")
        print("@c ---------------------------------------------------------------------")
        previous = l[1]
        i += 1
        if i == len(libraryClassifications):
            break
        l = libraryClassifications[i]
        if i+1 == len(libraryClassifications):
            next = ""
        else:
            next = libraryClassifications[i+1][1]

#
#  displayMenu - displays the top level menu for library documentation
#

def displayMenu():
    print("@menu")
    for l in libraryClassifications:
        print("* " + l[1] + "::" + l[2])
    print("@end menu")

    print("\n")
    print("@c =====================================================================")
    print("\n")


#
#  removeInitialComments - removes any (* *) at the top of the definition module
#

def removeInitialComments (file, line):
    while (str.find(line, "*)") == -1):
        line = file.readline()

#
#  removeFields - removes Author/Date/Last edit/SYSTEM/Revision fields from a comment within the start
#                 of a definition module
#

def removeFields (file, line):
    while (str.find(line, "*)") == -1):
        if (str.find(line, "Author") != -1) and (str.find(line, ":") != -1):
            line = file.readline()
        elif (str.find(line, "Last edit") != -1) and (str.find(line, ":") != -1):
            line = file.readline()
        elif (str.find(line, "LastEdit") != -1) and (str.find(line, ":") != -1):
            line = file.readline()
        elif (str.find(line, "Last update") != -1) and (str.find(line, ":") != -1):
            line = file.readline()
        elif (str.find(line, "Date") != -1) and (str.find(line, ":") != -1):
            line = file.readline()
        elif (str.find(line, "Title") != -1) and (str.find(line, ":") != -1):
            line = file.readline()
        elif (str.find(line, "Revision") != -1) and (str.find(line, ":") != -1):
            line = file.readline()
        elif (str.find(line, "System") != -1) and (str.find(line, ":") != -1) and (str.find(line, "Description:") == -1):
            line = file.readline()
        elif (str.find(line, "SYSTEM") != -1) and (str.find(line, ":") != -1) and (str.find(line, "Description:") == -1):
            line = file.readline()
        else:
           print(str.replace(str.replace(str.rstrip(line),
                                            "{", "@{"), "}", "@}"))
           line = file.readline()
    print(str.rstrip(line))


#
#  checkIndex
#

def checkIndex (line):
    global inVar, inType, inConst

    words = str.split(line)
    procedure = ""
    if (len(words)>1) and (words[0] == "PROCEDURE"):
        inConst = False
        inType = False
        inVar = False
        if (words[1] == "__BUILTIN__") and (len(words)>2):
            procedure = words[2]
        else:
            procedure = words[1]

    if (len(line)>1) and (line[0:2] == '(*'):
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
        words = str.split(line, ',')
        for word in words:
            word = str.lstrip(word)
            if word != "":
                if str.find(word, ':') == -1:
                    print("@findex " + word + " (var)")
                elif len(word)>0:
                    var = str.split(word, ':')
                    if len(var)>0:
                        print("@findex " + var[0] + " (var)")

    if inType:
        words = str.lstrip(line)
        if str.find(words, '=') != -1:
            word = str.split(words, "=")
            if (len(word[0])>0) and (word[0][0] != '_'):
                print("@findex " + str.rstrip(word[0]) + " (type)")
        else:
            word = str.split(words)
            if (len(word)>1) and (word[1] == ';'):
                # hidden type
                if (len(word[0])>0) and (word[0][0] != '_'):
                    print("@findex " + str.rstrip(word[0]) + " (type)")

    if inConst:
        words = str.split(line, ';')
        for word in words:
            word = str.lstrip(word)
            if word != "":
                if str.find(word, '=') != -1:
                    var = str.split(word, '=')
                    if len(var)>0:
                        print("@findex " + var[0] + " (const)")

    if procedure != "":
        name = str.split(procedure, "(")
        if name[0] != "":
            proc = name[0]
            if proc[-1] == ";":
                proc = proc[:-1]
            if proc != "":
                print("@findex " + proc)


#
#  parseDefinition
#

def parseDefinition (dir, source, build, file, needPage):
    print("")
    f = open(findFile(dir, build, source, file), 'r')
    initState()
    line = f.readline()
#   while (str.find(line, "(*") != -1):
    while (str.find(line, "(*") != -1):
        removeInitialComments(f, line)
        line = f.readline()

    while (str.find(line, "DEFINITION") == -1):
        line = f.readline()

    print("@example")
    print(str.rstrip(line))
    line = f.readline()
    if len(str.rstrip(line)) == 0:
        print(str.replace(str.replace(str.rstrip(line),
                                            "{", "@{"), "}", "@}"))
        line = f.readline()
        if (str.find(line, "(*") != -1):
            removeFields(f, line)
        else:
            print(str.rstrip(line))
    else:
        print(str.rstrip(line))

    line = f.readline()
    while line:
        line = str.rstrip(line)
        checkIndex(line)
        print(str.replace(str.replace(line, "{", "@{"), "}", "@}"))
        line = f.readline()
    print("@end example")
    if needPage:
        print("@page")
    f.close()

def parseModules (up, dir, build, source, listOfModules):
    previous = ""
    i = 0
    if len(listOfModules)>1:
        next = dir + "/" + listOfModules[1][:-4]
    else:
        next = ""

    while i<len(listOfModules):
       print("@node " + dir + "/" + listOfModules[i][:-4] + ", " + next + ", " + previous + ", " + up)
       print("@subsection " + dir + "/" + listOfModules[i][:-4])
       parseDefinition(dir, source, build, listOfModules[i], True)
       print("\n")
       previous = dir + "/" + listOfModules[i][:-4]
       i = i + 1
       if i+1<len(listOfModules):
           next = dir + "/" + listOfModules[i+1][:-4]
       else:
           next = ""


#
#  doCat - displays the contents of file, name, to stdout
#

def doCat (name):
    file = open(name, 'r')
    line = file.readline()
    while line:
        print(str.rstrip(line))
        line = file.readline()
    file.close()


#
#  moduleMenu - generates a simple menu for all definition modules
#               in dir
#

def moduleMenu (dir, build, source):
    print("@menu")
    listOfFiles = []
    if os.path.exists(os.path.join(source, dir)):
        listOfFiles += os.listdir(os.path.join(source, dir))
    if os.path.exists(os.path.join(source, dir)):
        listOfFiles += os.listdir(os.path.join(build, dir))
    listOfFiles = list(dict.fromkeys(listOfFiles).keys())
    listOfFiles.sort()
    for file in listOfFiles:
        if foundFile(dir, build, source, file):
            if (len(file)>4) and (file[-4:] == '.def'):
                print("* " + dir + "/" + file[:-4] + "::" + file)
    print("@end menu")
    print("\n")


#
#  checkDirectory - returns True if dir exists in either build or source.
#

def checkDirectory (dir, build, source):
    if os.path.isdir(build) and os.path.exists(os.path.join(build, dir)):
        return True
    elif os.path.isdir(source) and os.path.exists(os.path.join(source, dir)):
        return True
    else:
        return False


#
#  foundFile - return True if file is found in build/dir/file or source/dir/file.
#

def foundFile (dir, build, source, file):
    name = os.path.join(os.path.join(build, dir), file)
    if os.path.exists(name):
        return True
    name = os.path.join(os.path.join(source, dir), file)
    if os.path.exists(name):
        return True
    return False


#
#  findFile - return the path to file searching in build/dir/file first then source/dir/file.
#

def findFile (dir, build, source, file):
    name1 = os.path.join(os.path.join(build, dir), file)
    if os.path.exists(name1):
        return name1
    name2 = os.path.join(os.path.join(source, dir), file)
    if os.path.exists(name2):
        return name2
    print("file cannot be found in either " + name1 + " or " + name2)
    os.sys.exit(1)


#
#  displayModules - walks though the files in dir and parses
#                   definition modules and includes README.texi
#

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
                if (len(file)>4) and (file[-4:] == '.def'):
                    listOfModules += [file]
        listOfModules.sort()
        parseModules(up, dir, build, source, listOfModules)
    else:
        print("directory " + dir + " not found in either " + build + " or " + source)


def displayCopyright ():
    print("@c Copyright (C) 2000-2019 Free Software Foundation, Inc.")
    print("@c This file is part of GNU Modula-2.")
    print("""
@c Permission is granted to copy, distribute and/or modify this document
@c under the terms of the GNU Free Documentation License, Version 1.2 or
@c any later version published by the Free Software Foundation.
""")

def Usage():
    print("def2texi.py [-h][-bbuilddir][-uupnode][-ffilename]")

def collectArgs():
    buildDir="."
    sourceDir="."
    filename=""
    up=""
    try:
        optlist, list = getopt.getopt(sys.argv[1:],':hb:f:s:u:')
    except getopt.GetoptError:
        Usage()
        os.sys.exit(1)
    for opt in optlist:
        if opt[0] == '-h':
            Usage()
        if opt[0] == '-b':
            buildDir = opt[1]
        if opt[0] == '-f':
            filename = opt[1]
        if opt[0] == '-s':
            sourceDir = opt[1]
        if opt[0] == '-u':
            up = opt[1]
    return buildDir, sourceDir, filename, up


buildDir, sourceDir, filename, up = collectArgs()

if filename == "":
    displayCopyright()
    displayMenu()
    displayLibraryClass()
else:
    parseDefinition('.', sourceDir, buildDir, filename, False)
