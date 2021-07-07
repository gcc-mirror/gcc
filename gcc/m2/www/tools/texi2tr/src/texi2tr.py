#!/usr/bin/python3

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

import sys
import os
import glob
import string
import getopt
import outputdev
import navigation
import config

verbose = False
debugging = False
lineNo = 0
filename = ""
argStack = []
functions = {}
endFunctions = {}
values = {}
statementStack = {}
includePath = './'
templatePath = '.'
title = ""
inTitlePage = False
nodeName = False
tableSpecifier = []
enumerateSpecifier = []
menuText = ""
macroText = ""
frags = []
outputFile = sys.stdout.write
rootName = ""
baseName = "texi2tr-%d.html"
indexFunc = {}
indexSections = {}
html = None
macroDef = {}
macroContent = {}
currentMacro = None
subDirectory = "."


# output state
ignore, passthrough, arguments, menu, macro = list (range (5))

currentMenu = navigation.menuInfo (True)



#
#  printf - keeps C programmers happy :-)
#

def printf (format, *args):
    print(str(format) % args, end=' ')


#
#  debugf - issues prints if debugging is set
#

def debugf (format, *args):
    global debugging
    if debugging:
        print(str(format) % args, end=' ')


#
#  verbosef - issues prints if verbose is set
#

def verbosef (format, *args):
    global verbose
    if verbose:
        print(str(format) % args, end=' ')


#
#  Usage - displays the usage
#

def Usage ():
    print("texi2tr [-h] [-v] [-Iincludepath] [-s subdirectory] [-Ttemplatepath] [-r rootname.html] [-b basename-%d.html] filename.texi")
    print("  produces html from the texinfo filename.texi")
    print("  -s subdirectory    place html pages generated into subdirectory")
    print("  -h help")
    print("  -v verbose")
    sys.exit(0)


#
#  collectArgs - collects the arguments supplied and places
#                useful contents into global variables.
#

def collectArgs():
    global verbose, debugging, includePath, templatePath, rootName, baseName, nodeName, subDirectory
    try:
        optlist, list = getopt.getopt(sys.argv[1:],':b:dhI:nr:s:T:v')
    except getopt.GetoptError:
        Usage()
    for opt in optlist:
        if opt[0] == '-d':
            debugging = True
        if opt[0] == '-h':
            Usage()
        if opt[0] == '-v':
            verbose = True
        if opt[0] == '-I':
            includePath = opt[1]
        if opt[0] == '-T':
            templatePath = opt[1]
        if opt[0] == '-r':
            rootName = opt[1]
        if opt[0] == '-s':
            subDirectory = opt[1]
        if opt[0] == '-b':
            baseName = opt[1]
        if opt[0] == '-n':
            nodeName = True
    return list


#
#  readFile - returns the contents of a file, but will exit gracefully
#             if filename does not exist.
#

def readFile (filename):
    global includePath
    try:
        for i in includePath.split(':'):
            f = os.path.join(i, filename)
            if os.path.exists(f) and os.path.isfile(f):
                debugf('opening file: %s\n', f)
                return open(f, 'r').read()
        printf("cannot find file: %s\n", filename)
        sys.exit(0)
    except:
        printf("cannot open: %s\n", filename)
        sys.exit(0)


#
#  doState - determine whether to emit contents or not.
#

def doState(contents, state):
    global lineNo, html, menu, menuText, macroText
    debugf('doState')
    if state == passthrough:
        html.write(contents)
    elif state == arguments:
        argStack[0] += contents
    elif state == menu:
        menuText += contents
    elif state == macro:
        macroText += contents

#
#  skipLine - removes the first line from contents.
#

def skipLine (contents, state):
    debugf('skipLine')
    i = contents.find('\n')
    if (i>=0) and (i<len(contents)):
        doState(contents[:i+1], state)
        return contents[i+1:], state
    else:
        return "", state


#
#  error - issue an error and exit
#

def error (message):
    global filename, lineNo
    sys.stderr.write('%s:%d  %s\n' % (filename, lineNo, message))
    sys.exit(1)

#
#  warning - issue a warning
#

def warning (message):
    global filename, lineNo
    sys.stderr.write('%s:%d  warning %s\n' % (filename, lineNo, message))


#
#  pushArg - place init at the top of the argStack.
#            (head of the list)
#

def pushArg (init):
    global argStack, debugging
    argStack = [init] + argStack
    if debugging:
        print("pushArg:  argStack", argStack)


#
#  popArg - return the top of the argStack.
#           (remove head of the list and return it)
#

def popArg ():
    global argStack, debugging
    value = argStack[0]
    if len(argStack)>1:
        argStack = argStack[1:]
    else:
        argStack = []
    if debugging:
        print("popArg:  argStack", argStack)
    return value


#
#  findArgend - return the end position denoting the end of the
#               argument string.
#

def findArgend (contents):
    debugf('findArgend')
    i = 0
    level = 0
    while (i<len(contents)) and ((contents[i] != '}') or (level != 0)):
        if contents[i] == '{':
            level += 1
        elif (contents[i] == '@') and (len(contents)>i+1) and ((contents[i+1] == '{') or (contents[i+1] == '}')):
            i += 1
        elif contents[i] == '}':
            level -= 1
        i += 1
    return i


def stop():
    pass

#
#  parseArgs
#

def parseArgs (contents, i, delim):
    debugf('parseArgs')
    if contents[i] == ' ':
        contents = contents[i:]
        contents = contents.lstrip(' ')
        pushArg("")
        i = contents.find('\n')
        args = contents.split('\n')[0]
    elif contents[i] == '{':
        contents = contents[i+1:]
        pushArg("")
        i = findArgend(contents)
        args = contents[:i]
    if len(contents)>i:
        return contents[i+1:], args
    else:
        return "", args


def endMacroSeen (content, args):
    if (content == "end") and (args == "macro"):
        return (content == "end") and (args == "macro")
    return False


#
#  substitute - given a macro, command, substute its parameters
#               with args.
#

def substitute (command, args):
    contents = macroContent[command]
    formalParameters = macroDef[command]
    for count, param in enumerate (formalParameters.split (",")):
        param = param.rstrip ()
        param = param.lstrip ()
        if count < len (args):
            contents = contents.replace ("\\" + param + "\\", args[count])
    return contents


#
#  call - call the function, command, with args and state.
#

def call (command, args, state):
    global functions, macroDef, macroContent, macroText

    # printf ("command = %s,  state = %d\n", command, state)
    if (state == macro) and (not endMacroSeen (command, args)):
        macroText += "@"
        macroText += command
        macroText += "{"
        macroText += args
        macroText += "}"
        return "", state
    elif command in macroContent:
        # need to substitute args into macro
        return substitute (command, args), state
    elif command in functions:
        return functions[command](args, state)
    else:
        # printf ("********* unknown command %s\n", command)
        # error("unknown command '" + command + "'")
        return "", state

#
#  collectNonProcessedArgs - returns the arguments but without
#                            processing @ symbols.
#

def collectNonProcessedArgs (command, contents):
    if (command == 'c') or (command == 'comment'):
        i = contents.find('\n')
        if len(contents)<i:
            return "", ""
        else:
            contents = contents[i+1:]
            return contents, ""
    elif command == 'table':
        i = contents.find(' ')
        if (len(contents)<i) or (i == -1):
            return "", ""
        else:
            contents = contents[i:]
            i = contents.find('\n')
            if len(contents)<i:
                return "", ""
            else:
                args = contents[:i].lstrip(' ').rstrip(' ')
                contents = contents[i+1:]
                return contents, args


#
#  switchAt - extracts the command and argument and proceeds to
#             call the appropriate function.
#

def switchAt (contents, state):
    global macroText

    debugf('switchAt')
    if len (contents) > 0:
        return prepareCall (contents, state)
    debugf('return final switchAt')
    return contents, state


def prepareCall (contents, state):
    contents = contents[1:]
    if (contents[0] == '@') or (contents[0] == '{') or (contents[0] == '}') or (contents[0] == '.'):
        if state==arguments:
            # still need to escape it
            doState('@', state)
        doState(contents[0], state)
        debugf('return 1st switchAt')
        return contents[1:], state
    else:
        # search for space or {
        l = contents.find('\n')
        if l>=0:
            i = contents.find(' ', 0, l)
            j = contents.find('{', 0, l)
        else:
            i = contents.find(' ')
            j = contents.find('{')
        if (i>=0) and ((j == -1) or (i<j)):
            command = contents.split()[0]
            if command in ['c', 'comment']:
                contents, args = collectNonProcessedArgs(command, contents)
                return contents, state
            elif command in ['table']:
                contents, args = collectNonProcessedArgs(command, contents)
                c, s = call(command, args, state)
                contents = c + contents
                return contents, s
            contents, args = parseArgs(contents, i, '\n')
            debugf('space delimited' + command + "<" + args + ">")
            debugf('return 3rd switchAt')
            c, s = call(command, args, state)
            contents = c + contents
            return contents, s
        elif (j>=0) and ((i == -1) or (j<i)):
            command = contents.split('{')[0]
            contents, args = parseArgs(contents, j, '}')
            debugf('brace delimited' + command + "<" + args + ">")
            debugf('return 4th switchAt')
            c, s = call(command, args, state)
            contents = c + contents
            return contents, s
        else:
            command = contents.split()[0]
            i = contents.find('\n')
            if (i>=0) and (i<len(contents)):
                contents = contents[i+1:]
            args = ""
            debugf(command + "<" + args + ">")
            debugf('return 4th switchAt')
            c, s = call(command, args, state)
            contents = c + contents
            return contents, s



#
#  scanFor - search for '@' and parse this command.
#

def scanFor (contents, state):
    debugf('scanFor')
    i = contents.find('@')
    while i>=0:
        doState(contents[:i], state)
        contents, state = switchAt(contents[i:], state)
        i = contents.find('@')
    doState(contents, state)
    return "", state


#
#  parseFile -
#

def parseFile (contents, state):
    global html
    debugf('parseFile')
    contents, state = skipLine(contents, state)
    contents, state = scanFor(contents, state)
    return contents, state


#
#  doComment -
#

def doComment (content, state):
    return "", state

def doSetfilename (content, state):
    return "", state

def doInclude (content, state):
    return readFile(content), state

def doSet (content, state):
    global values
    if len(content.split())>2:
        des, expr = content.split()[0:2]
    elif len(content.split()) == 2:
        des, expr = content.split()
    else:
        des = content.split ()[0]
        expr = True
    values[des] = expr
    return "", state

def doValue (content, state):
    global values
    if content in values:
        return values[content], state
    else:
        error('unknown value ' + content)
    return "", state

def doSettitle (content, state):
    return "", state

def doTitlepage (content, state):
    global inTitlePage
    inTitlePage = True
    return doConsume(content, state, 'titlepage')

def doTitlepageEnd (state):
    global html, title, inTitlePage
    inTitlePage = False
    html.setTitle(title)
    html.paraBegin()

def doCenter (content, state):
    global html, title, inTitlePage

    if inTitlePage:
        return content, state
    elif state == ignore:
        return "", state
    else:
        html.centerBegin()
        html.write(content)
        html.centerEnd()
        return "", state

def doTitlefont (content, state):
    global title, inTitlePage
    if inTitlePage and (state != ignore) and (title == ""):
        pushArg("")
        scanFor(content, arguments)
        title = popArg()
    return "", state

def doI (content, state):
    global html
    if state != ignore:
        html.push()
        html.iBegin()
        scanFor(content, state)
        html.iEnd()
        html.pop()
    return "", state

def doStrong (content, state):
    global html
    if state != ignore:
        html.push()
        html.bBegin()
        scanFor(content, state)
        html.bEnd()
        html.pop()
    return "", state

def doVar (content, state):
    global html
    if state != ignore:
        html.push()
        html.iBegin()
        scanFor(content, state)
        html.iEnd()
        html.pop()
    return "", state

def doSamp (content, state):
    global html
    if state != ignore:
        html.push()
        html.ttBegin()
        scanFor(content, state)
        html.ttEnd()
        html.pop()
    return "", state

def doCode (content, state):
    global html
    if state != ignore:
        html.push()
        html.ttBegin()
        scanFor(content, state)
        html.ttEnd()
        html.pop()
    return "", state

def doBr (content, state):
    global html
    return "", state
    # html.br ()

#
#  doQuotation -
#

def doQuotation (content, state):
    return doConsume(content, state, 'quotation')

def doIfinfo (content, state):
    if state == ignore:
        return doConsume(content, state, 'ifinfo')
    else:
        pushState('ifinfo', state)
        return '', ignore

def doIfhtml (content, state):
    if state == ignore:
        return doConsume(content, state, 'ifhtml')
    else:
        pushState('ifhtml', state)
        return '', state

def doIftex (content, state):
    if state == ignore:
        return doConsume(content, state, 'iftex')
    else:
        pushState('iftex', state)
        return '', state

def doIfNottex (content, state):
    if state == ignore:
        return doConsume(content, state, 'ifnottex')
    else:
        pushState('ifnottex', state)
        return '', state

def doIfSet (content, state):
    # printf ("ifset %s\n", content)
    if state == ignore:
        return doConsume(content, state, 'ifset')
    else:
        pushState('ifset', state)
        # print ("values = ", values)
        if content in values:
            # printf ("values contains %s\n", content)
            return '', state
        else:
            # printf ("values does not contain %s\n", content)
            return '', ignore

def doIfClear (content, state):
    if state == ignore:
        return doConsume(content, state, 'ifclear')
    else:
        if content in values:
            return doConsume(content, ignore, 'ifclear')
        else:
            pushState('ifclear', state)
            return '', state

def doCopying (content, state):
    if state == ignore:
        return doConsume(content, state, 'copying')
    else:
        html.end()
        pushState('copying', state)
        html.paraBegin()
        return '', state

def doCopyingEnd (state):
    global html
    if state != ignore:
        html.end()   # end of paragraph

def doPass (content, state):
    return content, state

def doIgnoreTag (content, state):
    return "", state

def doIgnore (content, state):
    pushState ('ignore', state)
    return "", ignore

def doIgnoreEnd (state):
    pass

def doFormat (content, state):
    pushState ('format', state)
    return "", state

def doFormatEnd (state):
    pass

def doDirentry (content, state):
    pushState ('direntry', state)
    return "", state

def doDirentryEnd (state):
    pass

def pushState (keyword, state):
    global statementStack
    if keyword in statementStack:
        statementStack[keyword] = [state] + statementStack[keyword]
    else:
        statementStack[keyword] = [state]

def popState (keyword):
    state = statementStack[keyword][0]
    # printf ("statement stack  %s\n", statementStack[keyword])
    if len (statementStack[keyword]) == 1:
        statementStack[keyword] = []
    else:
        statementStack[keyword] = statementStack[keyword][1:]
    return state

def doConsume (content, state, keyword):
    pushState(keyword, state)
    return content, ignore

def doEnd (content, state):
    global statementStack, endFunctions
    keyword = content.split ()[0]
    if keyword in statementStack:
        if len (statementStack[keyword]) == 0:
            error ("unexpected end '" + keyword + "'")
        else:
            if keyword in endFunctions:
                # printf ("end %s\n", keyword)
                endFunctions[keyword] (state)
            else:
                # printf ("end function %s missing\n", keyword)
                pass
            # printf ("end %s: old state = %d", keyword, state)
            state = popState (keyword)
            # printf (", new state = %d", state)
    else:
        error ("'@end " + keyword + "'" + " without '@" + keyword + "'")
    return "", state

def doExampleEnd (state):
    global html
    if state != ignore:
        html.preEnd()
        html.paraBegin()

def doCopyright (content, state):
    global html

    if state != ignore:
        html.copyright()
    return content, state

#
#  sefeName
#

def safeName (name):
    name = name.replace ("%", "")
    name = name.replace ("^", "")
    name = name.replace (")", "")
    name = name.replace ("(", "")
    name = name.replace ("[", "")
    name = name.replace ("]", "")
    name = name.replace ("{", "")
    name = name.replace ("}", "")
    name = name.replace ("<", "")
    name = name.replace (">", "")
    name = name.replace ("'", "")
    name = name.replace ("?", "")
    name = name.replace ("*", "")
    name = name.replace ("/", "")
    name = name.replace ('"', "")
    name = name.replace ("'", "")
    name = name.replace ("`", "")
    name = name.replace ("~", "")
    name = name.replace (" ", "_")
    return name.lower()

def doNode (content, state):
    global frags
    if state == ignore:
        return skipLine (content, state)
    label = content.split(',')[0].rstrip().lstrip()
    if label == "Top":
        return "", state
    frags += [['text', html.collectFragment('node')]]
    name = safeName (label) + '.html'
    html.setFilename (name)
    # print ("node name =", name)
    frags += [['text', html.collectFragment('node')]]
    frags += [['node', navigation.addNode(html, content)]]
    return "", state

def doTop (content, state):
    if state == ignore:
        return skipLine (content, state)
    return "", state

def doMenu (content, state):
    global currentMenu, html, menuText, frags
    if state == ignore:
        return doConsume(content, state, 'menu')
    else:
        frags += [['text', html.collectFragment('menu')]]
        pushState('menu', state)
        menuText = ""
        return "", menu

def doMenuEnd (state):
    global currentMenu, frags
    if state != ignore:
        currentMenu.parseMenu(menuText)
    frags += [['menu', currentMenu]]
    currentMenu = navigation.menuInfo(False)
    return "", state

def doChapter (content, state):
    global html
    if state == ignore:
        return skipLine (content, state)
    html.h2Begin()
    html.write(content)
    html.h2End()
    html.paraBegin()
    return "", state

def addSectionAnchor (content):
    global html, indexSections
    if content in indexSections:
        error('section name "' + content + '" already exists')
    else:
        indexSections[content] = html.sectionAnchor(content)

def doSection (content, state):
    global html
    if state == ignore:
        return skipLine (content, state)
    addSectionAnchor(content)
    html.h3Begin()
    html.write(content)
    html.h3End()
    html.paraBegin()
    return "", state

def doSubSection (content, state):
    global html, indexSections
    if state == ignore:
        return skipLine (content, state)
    addSectionAnchor(content)
    html.h4Begin()
    html.write(content)
    html.h4End()
    html.paraBegin()
    return "", state

def doUref (content, state):
    global html
    if state != ignore:
        html.raw('<a href="')
        p = content.split(',')
        if len(p) == 1:
            scanFor(content, state)
            html.raw('">')
            scanFor(content, state)
        elif len(p) == 2:
            scanFor(p[0].rstrip().lstrip(), state)
            html.raw('">')
            scanFor(p[1].rstrip().lstrip(), state)
        elif len(p) >= 2:
            scanFor(p[0].rstrip().lstrip(), state)
            html.raw('">')
            scanFor(p[2].rstrip().lstrip(), state)
        html.raw('</a>')
    return "", state

def doEmail (content, state):
    global html
    if state != ignore:
        html.raw('<a href="mailto:')
        scanFor(content, state)
        html.raw('">')
        scanFor(content, state)
        html.raw('</a>')
    return "", state

#
#  pushSpecifier - pushes a format to the top of stack.
#

def pushSpecifier (dataType, format):
    global tableSpecifier
    tableSpecifier = [[dataType, format]] + tableSpecifier

#
#  popSpecifier - removes the top of stack
#

def popSpecifier ():
    global tableSpecifier
    if len(tableSpecifier)>1:
        tableSpecifier = tableSpecifier[1:]
    else:
        tableSpecifier = []

#
#  getSpecifier - returns the top of stack.
#

def getSpecifier ():
    global tableSpecifier
    return tableSpecifier[0]

def isTableSpecifier (s):
    return s[0] == 'table'

def isEnumSpecifier (s):
    return s[0] == 'enumerate'

def isItemizeSpecifier (s):
    return s[0] == 'itemize'

def is_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

def incrementSpecifier ():
    global tableSpecifier
    if is_int(tableSpecifier[0][1]):
        tableSpecifier[0][1] = "%d" % (int(tableSpecifier[0][1])+1)
    else:
        tableSpecifier[0][1] = chr(ord(tableSpecifier[0][1])+1)

#
#  doItemize - collect the format specifier and emit the start
#

def doItemize (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'itemize')
    else:
        html.end()
        pushSpecifier('itemize', content)
        pushState('itemize', state)
        html.itemizeBegin()
        html.noBegin()
        return "", state

#
#  doItemizeEnd - terminates the current itemize
#

def doItemizeEnd (state):
    global html
    if state != ignore:
        html.noEnd()
        popSpecifier()
        html.itemizeEnd()
        html.paraBegin()

#
#  doMacro - define a texi macro.
#

def doMacro (content, state):
    global html, currentMacro, macroDef
    if state == ignore:
        return doConsume (content, state, 'macro')
    else:
        # print ("macro seen:  defn = ", content)
        currentMacro = content.split ("{")[0]
        macroDef[currentMacro] = content
        pushState ('macro', state)
        return "", macro

#
#  doMacroEnd - terminates the current macro.
#

def doMacroEnd (state):
    global macroText, macroContent, currentMacro
    if state == macro:
        macroContent[currentMacro] = macroText
        print ("macro defined: ", currentMacro, "contents", macroText)
        currentMacro = None
        macroText = ""


#
#  doTable - collect the format specifier and emit the start
#

def doTable (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'table')
    else:
        html.end()
        pushSpecifier('table', content)
        pushState('table', state)
        html.tableBegin()
        html.paraBegin()
        return "", state

#
#  doTableEnd - terminates the current table
#

def doTableEnd (state):
    global html
    if state != ignore:
        html.end()   # end of paragraph
        popSpecifier()
        html.tableRightEnd()
        html.tableEnd()
        html.paraBegin()

#
#  doItem -
#

def doItem (content, state):
    global html
    if state == ignore:
        return skipLine (content, state)
    else:
        if isItemizeSpecifier(getSpecifier()):
            html.noEnd()
            html.item()
            html.noBegin()
        else:
            html.end()  # shuts down a para
            html.tableRightEnd()
            html.tableLeftBegin()
            if isEnumSpecifier(getSpecifier()):
                stop()
                scanFor(getSpecifier ()[1] + ' ' + content, state)
                incrementSpecifier()
            else:
                scanFor(getSpecifier ()[1] + '{' + content + '}', state)
                html.tableLeftEnd()
            html.tableRightBegin()
            html.paraBegin()
        return "", state


#
#  doAlias - alias one macro by another.
#

def doAlias (content, state):
    if state == ignore:
        return skipLine (content, state)
    else:
        # --fixme-- this needs to be completed.
        return "", state

#
#  doTex - perform commands in tex.
#

def doTex (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'tex')
    else:
        pushState('tex', state)
        return "", ignore

#
#  doTexEnd - terminates the tex statement sequence.
#

def doTexEnd (state):
    global html
    if state != ignore:
        pass

#
#
#

def doBullet (content, state):
    global html
    if state == ignore:
        return skipLine (content, state)
    else:
        html.end()  # shuts down a para
        html.tableRightEnd()
        html.tableLeftBegin()
        scanFor(content, state)
        html.tableLeftEnd()
        html.tableRightBegin()
        html.paraBegin()
        return "", state


#
#  doEnumerate - collect the enumeration specifier and emit the start
#

def doEnumerate (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'enumerate')
    else:
        html.end()
        pushSpecifier('enumerate', content)
        pushState('enumerate', state)
        html.tableBegin()
        html.paraBegin()
        return "", state

#
#  doEnumerateEnd - terminates the current enumeration list.
#

def doEnumerateEnd (state):
    global html
    if state != ignore:
        html.end()   # end of paragraph
        popSpecifier()
        html.tableRightEnd()
        html.tableEnd()
        html.paraBegin()

def doExample (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'example')
    else:
        pushState('example', state)
        html.paraEnd()
        html.preBegin()
        return content, state

def doSmallExample (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'smallexample')
    else:
        pushState('smallexample', state)
        html.paraEnd()
        html.preBegin()
        return content, state

def doSmallExampleEnd (state):
    global html
    if state != ignore:
        html.preEnd()
        html.paraBegin()

def doFootnote (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'footnote')
    else:
        html.paraEnd()
        pushState('footnote', state)
        html.raw('<div class="footnote"><ul class="footnote"><li class="footnote">\n')
        html.paraBegin()
        scanFor(content, state)
        html.paraEnd()
        html.raw("\n</li></ul></div>\n")
        state = popState('footnote')
        html.paraBegin()
        return "", state

def doXref (content, state):
    if state == ignore:
        return "", state
    else:
        text = "See ("
        args = content.split (",")
        if len (args) > 1:
            text += args[0]
            text += ", "
            text += args[-1]
        elif len (args) == 1:
            text += args[0]
        text += ")"
        return text, state

def doDisplay (content, state):
    global html
    if state == ignore:
        return doConsume(content, state, 'display')
    else:
        pushState('display', state)
        html.paraEnd()
        html.raw("<div display>\n")
        html.paraBegin()
        return content, state

def doDisplayEnd (state):
    global html
    if state != ignore:
        html.paraEnd()
        html.raw("</div>\n")
        html.paraBegin()

def doHeading (content, state):
    global html
    if state == ignore:
        return skipLine (content, state)
    html.h2Begin()
    html.write(content)
    html.h2End()
    html.paraBegin()
    return "", state

def doFindex (content, state):
    global indexFunc, html

    if state == ignore:
        return skipLine (content, state)
    if content in indexFunc:
        indexFunc[content] += [html.getLink()]
    else:
        indexFunc[content] = [html.getLink()]
    s = '<a name="' + html.getAnchor() + '"></a>\n'
    html.raw(s)
    html.nextAnchor()
    return "", state

def sort_compare (a, b):
    return a < b

#
#
#

def doPrintIndex (content, state):
    global indexFunc, html, frags

    if state == ignore:
        return skipLine (content, state)
    if not (content in ['cp', 'fn']):
        error('printindex knows about cp and fn but not ' + content)
    frags += [['text', html.collectFragment(content)]]
    frags += [[content, html.collectFragment(content)]]
    return "", state

#
#  doBye - adds the remaining fragment to the list of frags.
#

def doBye (content, state):
    global frags
    frags += [['text', html.collectFragment('bye')]]
    return "", state


def generateFunctionIndex (html):
    global indexFunc

    html.openDiv().flushDiv()
    html.paraBegin()
    for k,v in sorted([(key, value) for (key,value) in list(indexFunc.items())]):
        html.write(k)
        html.write(': ')
        for n in range(len(v)):
            if n>0:
                html.write(', ')
            html.raw('<a href="')
            html.raw(v[n])
            html.raw('">')
            html.write('[%d]' % (n+1))
            html.raw('</a>')
        html.raw('.<br>\n')
    html.paraEnd()
    html.closeDiv()


def generateSectionIndex (html):
    global indexSections

    html.openDiv().flushDiv()
    html.paraBegin()
    html.write('Alphabetically sorted list of contents')
    html.raw('<br>\n')
    for k,v in sorted([(key, value) for (key,value) in list(indexSections.items())]):
        html.raw('<a href="')
        html.raw(v)
        html.raw('">')
        html.write(k)
        html.raw('</a>')
        html.raw('<br>\n')
    html.paraEnd()
    html.closeDiv()


#
#  populateFunctions - create the dictionary of commands.
#

def populateFunctions ():
    global functions
    functions['*'] = doBr
    functions['alias'] = doAlias
    functions['author'] = doIgnoreTag
    functions['bullet'] = doBullet
    functions['bye'] = doBye
    functions['c'] = doComment
    functions['center'] = doPass
    functions['chapter'] = doChapter
    functions['code'] = doCode
    functions['command'] = doSamp
    functions['comment'] = doComment
    functions['copying'] = doCopying
    endFunctions['copying'] = doCopyingEnd
    functions['copyright'] = doCopyright
    functions['dircategory'] = doIgnoreTag
    functions['direntry'] = doDirentry
    endFunctions['direntry'] = doDirentryEnd
    functions['display'] = doDisplay
    endFunctions['display'] = doDisplayEnd
    functions['email'] = doEmail
    functions['end'] = doEnd
    functions['enumerate'] = doEnumerate
    endFunctions['enumerate'] = doEnumerateEnd
    functions['example'] = doExample
    endFunctions['example'] = doExampleEnd
    functions['file'] = doSamp
    functions['finalout'] = doPass
    functions['findex'] = doFindex
    functions['footnote'] = doFootnote
    functions['format'] = doFormat
    endFunctions['format'] = doFormatEnd
    functions['heading'] = doHeading
    functions['i'] = doI
    functions['ignore'] = doIgnore
    endFunctions['ignore'] = doIgnoreEnd
    functions['image'] = doIgnoreTag
    functions['include'] = doInclude
    functions['insertcopying'] = doIgnoreTag
    functions['ifclear'] = doIfClear
    functions['ifhtml'] = doIfhtml
    functions['ifinfo'] = doIfinfo
    functions['iftex'] = doIftex
    functions['ifnottex'] = doIfNottex
    functions['ifset'] = doIfSet
    functions['item'] = doItem
    functions['itemize'] = doItemize
    endFunctions['itemize'] = doItemizeEnd
    functions['macro'] = doMacro
    endFunctions['macro'] = doMacroEnd
    functions['menu'] = doMenu
    endFunctions['menu'] = doMenuEnd
    functions['node'] = doNode
    functions['page'] = doPass
    functions['printindex'] = doPrintIndex
    functions['summarycontents'] = doPass
    functions['contents'] = doPass
    functions['quotation'] = doQuotation
    functions['value'] = doValue
    functions['var'] = doVar
    functions['samp'] = doSamp
    functions['sc'] = doStrong
    functions['section'] = doSection
    functions['set'] = doSet
    functions['setchapternewpage'] = doPass
    functions['setfilename'] = doSetfilename
    functions['settitle'] = doSettitle
    functions['smallbook'] = doPass
    functions['smallexample'] = doSmallExample
    endFunctions['smallexample'] = doSmallExampleEnd
    functions['sp'] = doPass
    functions['strong'] = doStrong
    functions['subsection'] = doSubSection
    functions['subtitle'] = doPass # doSubTitle
    functions['table'] = doTable
    endFunctions['table'] = doTableEnd
    functions['tex'] = doTex
    endFunctions['tex'] = doTexEnd
    functions['title'] = doIgnoreTag
    functions['titlepage'] = doTitlepage
    endFunctions['titlepage'] = doTitlepageEnd
    functions['titlefont'] = doTitlefont
    functions['top'] = doTop
    functions['unnumbered'] = doChapter
    functions['unnumberedsec'] = doSection
    functions['uref'] = doUref
    functions['url'] = doUref
    functions['versionsubtitle'] = doIgnoreTag
    functions['vskip'] = doPass
    functions['xref'] = doXref

def openDiv (openedDiv):
    if not openedDiv:
        html.raw(' <div class="page">\n')
        html.raw('  <div class="plain">\n')
    return True

def closeDiv (openedDiv):
    if openedDiv:
        html.raw('  </div>\n')
        html.raw(' </div>\n')
    return False

#
#  mergeMenu - write out each menu between each fragment.
#

def mergeMenu ():
    global frags, html, outputFile

    root = navigation.getRoot(frags)
    fn = ""
    html.openFragment()
    selected = ""
    for f in frags:
        if f[0] == 'menu':
            if f[1] == root:
                pass
            elif not f[1]._isShort():
                html.openDiv().flushDiv()
                selected = f[1].generateMenu(html, root, selected)
                html.closeDiv()
        elif f[0] == 'node':
            if f[1] == root:
                pass
            else:
                html.flushDiv()
                html.nextFragment(fn)
                selected = f[1].generateMenu(html, root, selected)
        elif f[0] == 'text':
            if (fn != f[1][0]) and (f[1][0] != ""):
                fn = f[1][0]
            html.openDiv().flushDiv()
            html.raw(f[1][1])
            html.closeDiv()
        elif f[0] == 'fn':
            generateFunctionIndex(html)
        elif f[0] == 'cp':
            generateSectionIndex(html)
    html.flushDiv()
    html.closeFragment()

#
#  main - the first procedure to execute.
#

def main():
    global filename, baseName, rootName, templatePath, html, nodeName, subDirectory

    list = collectArgs()
    filename = list[0]
    html = outputdev.htmlDevice (rootName)
    html.setBasename (baseName)
    html.setTemplatePath (templatePath)
    html.setNodename (nodeName)
    html.setSubDirectory (subDirectory)

    populateFunctions()
    contents = readFile(filename)
    contents, state = parseFile(contents, passthrough)
    mergeMenu()

main()
