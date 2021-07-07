#!/usr/bin/python3

# array2index.py utility to convert static arrays into a dynamic equivalent.

# Copyright (C) 2011-2021 Free Software Foundation, Inc.
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
#

import sys
import os
import glob
import string
import getopt


lines = []   # global copy of the input lines of text.


#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print(str(format) % args, end=' ')


#
#  fatal - displays a message and exits
#

def fatal (format, *args):
    print(str(format) % args, end=' ')
    os.sys.exit(1)

#
#  debug
#

def debug(s):
    print("*", s, "*")

#
#  putNext - pushes, i, to be the next line read when
#            getNext is called.
#

def putNext (i):
    global lines
    lines = [i] + lines

#
#  getNext - returns the next line of input text.
#

def getNext ():
    global lines

    if lines==[]:
        return '<eof>'
    else:
        l = lines[0]
        lines = lines[1:]
        return l


#
#  isEof - return True if <eof> is seen.
#

def isEof (i):
    return i=='<eof>'


#
#  isProcedure - return True if 'PROCEDURE' is seen.
#

def isProcedure (i):
    l = i.split()
    return (len(l)>0) and (l[0]=='PROCEDURE')


#
#  isVar - return True if 'VAR' is seen.
#

def isVar (i):
    l = i.split()
    return (len(l)>0) and (l[0]=='VAR')


#
#  isBegin - return True if 'BEGIN' is seen.
#

def isBegin (i):
    l = i.split()
    return (len(l)>0) and (l[0]=='BEGIN')


#
#  isEnd - return True if the 'END' to a procedure is seen.
#

def isEnd (i):
    return (len(i)>3) and (i[0:3]=='END')


#
#  getVarIndent - return the variable indent (where ':' occurs)
#

def getVarIndent (l):
    n = 0
    while len(l)>n:
        if l[n]==':':
            return n
        n += 1
    return 0


#
#  getMaxIndent - return the maximum variable indent (where ':' occurs)
#

def getMaxIndent (v):
    max = 0
    for l in v:
        n = getVarIndent(l)
        if max<n:
            max = n
    return max

#
#  setVarIndent - sets the indentation to, n, for each variable declared.
#

def setVarIndent (v, n):
    w = []
    for l in v:
        i = l.find(':')
        if (i>=0) and (i<n):
            w += [l[0:i] + (' '*(n-i)) + l[i:]]
        else:
            w += [l]
    return w

#
#  adjustVar - adds the dictionary contents to the variable list.
#

def adjustVar (v, d):
    print(v, d)
    if d != {}:
        if v == []:
            h = ['VAR\n']
            t = []
            if 'pCall' in d:
                v = h + ['   pCall: PtrToCallFrame ;\n'] + t
            if 'pSym' in d:
                v = h + ['   pSym: PtrToSymbol ;\n'] + t
        else:
            h = v[0]
            if 'pCall' in d:
                v = [h] + ['   pCall: PtrToCallFrame ;\n'] + v[1:]
            if 'pSym' in d:
                v = [h] + ['   pSym: PtrToSymbol ;\n'] + v[1:]
        v = setVarIndent(v, getMaxIndent(v))
    return v

#
#  getIndent - returns the number of spaces before the text.
#

def getIndent (i):
    n = 0
    while len(i)>n:
        if i[n]==' ':
            n += 1
        else:
            return n
    return n

#
#  scanStatements - returns the statements in a procedure after they
#                   have been transformed to use an index rather than
#                   an array.
#

def scanStatements ():
    debug(scanStatements)
    s = []
    d = {}
    i = getNext()
    while not isEnd(i):
        x = i.find('Symbols[')
        if x>=0:
            n = getIndent(i)
            y = i.find('[', x)+1
            z = i.find(']', y)
            print("indexing ", i[y:z])
            d['pSym'] = i[y:z]
            j = n * ' '
            j += 'pSym := GetPsym(%s) ;\n' % i[y:z]
            s += [j]
            i = i[0:x]+'pSym^'+i[z+1:]
        else:
            x = i.find('ScopeCallFrame[')
            if x>=0:
                n = getIndent(i)
                y = i.find('[', x)+1
                z = i.find(']', y)
                print("indexing ", i[y:z])
                d['pCall'] = i[y:z]
                j = n * ' '
                j += 'pCall := GetPcall(%s) ;\n' % i[y:z]
                s += [j]
                i = i[0:x]+'pCall^'+i[z+1:]
        s += [i]
        i = getNext()
    putNext(i)
    return s, d

#
#  scanVar - returns the list of variable declarations.
#

def scanVar ():
    v = []
    i = getNext()
    while not isBegin(i):
        v += [i]
        i = getNext()
    putNext(i)
    return v

#
#  scanProcedure - returns the list of modified lines within
#                  a procedure.
#

def scanProcedure ():
    debug("scanProcedure")
    o = []
    v = []
    i = getNext()
    while (not isEnd(i)):
        if isVar(i):
            v = [i]
            v += scanVar()
            print(v)
        elif isBegin(i):
            s, d = scanStatements()
            v = adjustVar(v, d)
            o += v + [i] + s
            return o
        else:
            # const, type, comment
            o += [i]
        i = getNext()
    fatal("internal error")


#
#  scanLines - scans a list of lines for each procedure
#

def scanLines (l):
    global lines

    debug("scanLines")
    lines = l + ['<eof>']
    o = []
    i = getNext()
    while not isEof(i):
        o += i
        if isProcedure(i):
            print(i)
            o += scanProcedure()
        i = getNext()
    return o


#
#  Usage - display a single line summarising usage.
#

def Usage():
    print("array2index.py [-h][-o outputfile] inputfile")


#
#  collectArgs - return inputfile, outputfile.
#

def collectArgs():
    debug('collectArgs')
    inputfile="-"
    outputfile="-"
    try:
        optlist, list = getopt.getopt(sys.argv[1:], ':ho:')
        inputfile = list[0]
        for opt in optlist:
            if opt[0] == '-h':
                Usage()
                os.sys.exit(0)
            elif opt[0] == '-o':
                outputfile = opt[1]
    except getopt.GetoptError:
        Usage()
        os.sys.exit(1)
    return inputfile, outputfile


#
#  getFiles - given two strings, input, and, output, open and
#             return their two respective files.
#

def getFiles (inputfile, outputfile):
    if inputfile=='-':
        ip = sys.stdin
    else:
        if os.path.exists(inputfile):
            ip = open(inputfile, 'r')
        else:
            fatal('cannot open file %s', inputfile)
    if outputfile=='-':
        op = sys.stdout
    else:
        op = open(outputfile, 'w')
    return ip, op


#
#  main - the main procedure
#

def main():
    debug('main')
    inputfile, outputfile = collectArgs()
    ip, op = getFiles(inputfile, outputfile)
    op.writelines(scanLines(ip.readlines()))
    op.close()
    ip.close()


#
#  we start here
#

main()
