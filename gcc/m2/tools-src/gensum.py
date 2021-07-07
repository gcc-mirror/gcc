#!/usr/bin/env python3
# 
# gensum.py a utility for summarizing the regression tests into html.
# 
# Copyright (C) 2007-2021 Free Software Foundation, Inc.
# Contributed by Gaius Mulley <gaius@glam.ac.uk>.
# 
# This file is part of GNU Modula-2.
# 
# GNU Modula-2 is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# GNU Modula-2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU Modula-2; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.
#
import sys
import os
import glob
import string
import sys, getopt


class testcase:
    name = ""
    passes = []
    fails = []
    unresolved = []
    def __init__ (self, n):
        self.name = n
        self.passes = []
        self.fails = []
        self.unresolved = []
    def addPass (self, p):
        self.passes += [p]
    def addFail (self, p):
        self.fails += [p]
    def addUnresolved (self, p, r):
        self.unresolved += [p, r]
    def getPasses (self):
        return self.passes
    def getFails (self):
        return self.fails
    def getUnresolved (self):
        return self.unresolved


#
#  usage - displays the usage
#

def usage (code):
    global noColumns

    print("gensum [-h] [-c number] filename.sum {filename.sum}")
    print("  -c number of columns per architectural table (default", noColumns, ")")
    print("  -h help")
    sys.exit (code)


#
#  collectArgs - collects the arguments supplied and places
#                useful contents into global variables.
#

def collectArgs ():
    global noColumns
    try:
        optlist, list = getopt.getopt (sys.argv[1:],':hc:')
    except getopt.GetoptError:
        usage (1)
    for opt in optlist:
        if opt[0] == '-h':
            usage (0)
        if opt[0] == '-c':
            noColumns = int (opt[1])
    return list


#
#  scanner - generic function to read in a file, name,
#            and call, function, on each line read.
#

def scanner (name, function):
    file = open (name, 'r')
    line = file.readline ()
    while line:
        function (line)
        line = file.readline ()
    file.close ()

#
#  addPassResult - adds the pass information into the global dictionary.
#

def addPassResult (name, varient):
    global regressionTests, configuration, passStats

    if configuration not in regressionTests:
        regressionTests[configuration] = {}

    arch = regressionTests[configuration]
    if name in arch:
        t = arch[name]
    else:
        t = testcase (name)
    t.addPass (varient)
    arch[name] = t
    regressionTests[configuration] = arch
    if configuration in passStats:
        passStats[configuration] += 1
    else:
        passStats[configuration] = 1


#
#  addFailResult - adds the fail information into the global dictionary.
#

def addFailResult (name, varient):
    global regressionTests, configuration, failStats

    if configuration not in regressionTests:
        regressionTests[configuration] = {}

    arch = regressionTests[configuration]
    if name in arch:
        t = arch[name]
    else:
        t = testcase (name)
    t.addFail (varient)
    arch[name] = t
    regressionTests[configuration] = arch
    if configuration in failStats:
        failStats[configuration] += 1
    else:
        failStats[configuration] = 1



#
#  addUnresolvedResult - adds the unresolved information into the global dictionary.
#

def addUnresolvedResult (name, varient, reason):
    global regressionTests, configuration, unresolvedStats

    if configuration not in regressionTests:
        regressionTests[configuration] = {}

    arch = regressionTests[configuration]
    if name in arch:
        t = arch[name]
    else:
        t = testcase (name)
    t.addUnresolved (varient, reason)
    arch[name] = t
    regressionTests[configuration] = arch
    if configuration in unresolvedStats:
        unresolvedStats[configuration] += 1
    else:
        unresolvedStats[configuration] = 1


#
#  getName - returns the gm2 git testcase path
#

def getName (testcase, directory):
    words = string.split (directory, '/')
    result = ""
    found = False
    for word in words:
        if word == "testsuite":
            found = True
            result = word
        elif word == "gm2.exp":
            pass
        elif found:
            result = "%s/%s" % (result, word)
    words = string.split (testcase, '/')
    name = "%s/%s" % (result, words[-1])
    if name[-1] == ',':
        name = name[:-1]
    return name

#
#  processLine -
#

def processLine(line):
    global author, date, configuration, target, directory
    words = string.split (line)
    # Test Run By xxxx on
    if (len (words) >= 4) and (words[:3] == ["Test", "Run", "By"]):
        author = words[3]
        if (len (words) >= 6) and (words[4] == "on"):
            date = words[-5:]
    elif (len (words) >= 4) and (words[:3] == [ "Native", "configuration", "is"]):
        configuration = words[3]
    elif (len (words) >= 3) and (words[:2] == [ "Running", "target"]):
        target = words[2]
    elif (len (words) >= 2) and (words[0] == "Running"):
        directory = words[1]
    elif len(words)>1:
        testcase = words[1]
        varient = []
        reason = ""
        if testcase.find ('gm2/non-free') == -1:
            if words[0]=="PASS:":
                if len (words) >= 2:
                    varient = words[2:]
                addPassResult (getName (testcase, directory), varient)
            elif words[0]=="FAIL:":
                if len (words)>=2:
                    varient = words[2:]
                addFailResult (getName (testcase, directory), varient)
            elif words[0]=="UNRESOLVED:":
                if len (words) > 2:
                    start = -1
                    if words[-1][-1]==')':
                        while (-start < len (words)) and (words[start][0] != '('):
                            start -= 1
                    varient = words[2:start]
                    reason = words[start:]
                addUnresolvedResult (getName (testcase, directory), varient, reason)


#
#  printRow - prints out a table data entry for architecture and option.
#

def printRow (testcase, arch, option):
    if testcase in regressionTests[arch]:
        t = regressionTests[arch][testcase]
        if option in t.getPasses ():
            print('<td bgcolor="green">', string.join (option, ' '), '</td>', end=' ')
        elif option in t.getFails():
            print('<td bgcolor="red">', string.join (option, ' '), '</td>', end=' ')
        elif option in t.getUnresolved():
            print('<td bgcolor="yellow">', string.join (option, ' '), '</td>', end=' ')
        elif option == []:
            print('<td></td>', end=' ')
        else:
            print('<td></td>', end=' ')
    else:
        print('<td></td>', end=' ')


#
#  getListOfTests - returns the list of all tests
#

def getListOfTests ():
    global regressionTests

    list = []
    for arch in list(regressionTests.keys ()):
        t = regressionTests[arch]
        for u in list(t.keys ()):
            if not (u in list):
                list += [u]
    return list


#
#  getListOfOptions - returns the (total, optlist) for testcase
#                     in the regressionTests
#

def getListOfOptions (testcase):
    global regressionTests

    optlist = []
    total = 0
    for arch in list(regressionTests.keys ()):
        t = regressionTests[arch]
        if testcase in t:
            u = t[testcase]
            for p in u.getPasses () + u.getFails () + u.getUnresolved ():
                if not (p in optlist):
                    optlist += [p]
    return len (optlist), optlist


#
#  getHeading - returns a URL to the testcase.
#

def getHeading (testcase):
    noFiles = ['pimlib/ulm', 'pimlib/pass', 'ulmlib/pass', 'ulmlib/std',
               'ulmlib/sys', 'gm2/examples', 'gm2/non-free']
    for n in noFiles:
        if testcase.find (n) != -1:
            return testcase
    heading = '<a href="http://git.savannah.gnu.org/cgit/gm2.git/tree/gcc-versionno/gcc/%s' % testcase
    heading += '">'
    heading += testcase + '</a>'
    return heading

#
#  printResults - prints the resuls in a html tabular form
#

def printResults():
    global target, configuration, author, date, regressionTests, noColumns
    global passStats, failStats, unresolvedStats

    print("<html><head><title>")
    print("GNU Modula-2 regression tests")
    print("</title></head>")
    print("")
    print("<h1>", end=' ')
    print("GNU Modula-2 regression tests", end=' ')
    print("</h1>")
    print("")

    print('<p><table border="1"><tr>')
    print('<th colspan="2">Key</th>')
    print('<tr><td>Colour</td><td>Meaning</td></tr>')
    print('<tr><td bgcolor="green"></td><td>Pass</td></tr>')
    print('<tr><td bgcolor="red"></td><td>Fail</td></tr>')
    print('<tr><td bgcolor="yellow"></td><td>Unresolved due to a prior error</td></tr>')
    print('<tr><td bgcolor="blue"></td><td>Not tested</td></tr>')
    print('<tr><td></td><td>Entire testcase not tested on this platform</td></tr>')
    print('</table></p>')
    print('')

    archList = list(regressionTests.keys ())
    print("<h2>", end=' ')
    print("Summary", end=' ')
    print("</h2>")
    print('<p><table border="1">')
    print('<tr>', end=' ')
    print('<th colspan="1">Status</th>', end=' ')
    for arch in archList:
        print('<th colspan="1">', arch, '</th>', end=' ')
    print('</tr>')
    print('<tr><td bgcolor="green"></td>', end=' ')
    for arch in archList:
        if arch in passStats:
            print('<td bgcolor="green">', passStats[arch], "</td>")
        else:
            print('<td bgcolor="green">none</td>')
    print('</tr>')
    print('<tr><td bgcolor="red"></td>', end=' ')
    for arch in archList:
        if arch in failStats:
            print('<td bgcolor="red">', failStats[arch], "</td>")
        else:
            print('<td bgcolor="red">none</td>')
    print('</tr>')
    print('<tr><td bgcolor="yellow"></td>', end=' ')
    for arch in archList:
        if arch in unresolvedStats:
            print('<td bgcolor="yellow">', unresolvedStats[arch], "</td>")
        else:
            print('<td bgcolor="yellow">none</td>')
    print('</tr>')
    print('</table></p>')

    print("<h1>", end=' ')
    print("GNU Modula-2 regression test results", end=' ')
    print("</h1>")

    testlist = getListOfTests ()
    for testcase in testlist:
        total, optlist = getListOfOptions (testcase)
        if total > 0:
            print('<p><table border="1"><tr>')
            print('<th colspan="', len (archList) * noColumns, '">', end=' ')
            heading = getHeading (testcase)
            print(heading, '</th></tr>')
            for arch in archList:
                print('<th colspan="', noColumns, '">', arch, '</th>', end=' ')

            if total % noColumns != 0:
                total = ((total / noColumns) +1) * noColumns
            for count in range (0, total, noColumns):
                print('<tr>', end=' ')
                for arch in archList:
                    for c in range (count, count+noColumns):
                        if c < len (optlist):
                            printRow (testcase, arch, optlist[c])
                        else:
                            printRow (testcase, arch, [])
                print('</tr>')
            print('</table></p>')
    print('</html>')


target = ""
configuration = ""
author = ""
date = ""
regressionTests = {}
noColumns = 3
directory = ""
passStats = {}
failStats = {}
unresolvedStats = {}


#
#  main - collects the arguments and reads in each summary file
#         in turn populating the architecture dictionary in turn.
#

def main():
    global regressionTests

    filenames = collectArgs ()
    if filenames==[]:
        usage (0)
    else:
        for file in filenames:
            scanner (file, processLine)
        printResults ()

main()
