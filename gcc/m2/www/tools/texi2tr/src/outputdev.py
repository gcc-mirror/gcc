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
import string
import config


# html tag
null_tag, title_tag, header1_tag, header2_tag, header3_tag, header4_tag, center_tag, paragraph_tag, preformatted_tag, teletype_tag, italic_tag, bold_tag, no_tag = list(range(13))

# html state machine
init_state, known_state, end_state, copy_state, white_state, nf_state = list(range(6))

# char2code contains all html character codes
char2code = {}
char2code['"'] = "&quot;"
char2code['&'] = "&amp;"
char2code['<'] = "&lt;"
char2code['>'] = "&gt;"


# all methods prefixed by _ are for internal use only.

class htmlDevice:
    #
    #  setTemplatePath set the templatePath field.
    #
    def setTemplatePath (self, p):
        self.templatePath = p
    #
    #  setSubDirectory set the subDirectory field.
    #
    def setSubDirectory (self, d):
        print ("setting subDirectory to", d)
        self.subDirectory = d
    #
    #
    #
    def getNodeAnchor (self):
        if config.multipleFragments or config.debugFragments:
            return self.currentFilename
        else:
            return "SEC%d" % self.anchorCount
    #
    #
    #
    def getNodeLink (self):
        if config.multipleFragments or config.debugFragments:
            return self.currentFilename
        else:
            return "#SEC%d" % self.anchorCount
    #
    #
    #
    def getAnchor (self):
        if config.multipleFragments:
            if self.anchorCount == 0:
                return self.currentFilename
            else:
                if config.debugFragments:
                    return ('<<' + self.currentFilename + ">> SEC%d") % self.anchorCount
                else:
                    return "SEC%d" % self.anchorCount
        else:
            return "SEC%d" % self.anchorCount
    #
    #  nextAnchor
    #
    def nextAnchor (self):
        self.anchorCount += 1
    #
    #
    #
    def getLink (self):
        if config.multipleFragments:
            if self.anchorCount == 0:
                return self.currentFilename
            else:
                if config.debugFragments:
                    return ('<<' + self.currentFilename + ">> #SEC%d") % self.anchorCount
                else:
                    return "%s#SEC%d" % (self.currentFilename, self.anchorCount)
        else:
            return "#SEC%d" % self.anchorCount
    #
    #  openDiv - delay opening a div
    #
    def openDiv (self):
        self.pendingDiv = True
        return self
    #
    #  closeDiv - delay closing a div
    #
    def closeDiv (self):
        self.pendingDiv = False
        return self
    #
    #  flushDiv - really write the div needed.
    #
    def flushDiv (self):
        if self.pendingDiv == self.actualDiv:
            return self
        self.actualDiv = self.pendingDiv
        if self.pendingDiv:
            self.raw(' <div class="page">\n')
            self.raw('  <div class="plain">\n')
        else:
            self.raw('  </div>\n')
            self.raw(' </div>\n')
        return self
    #
    #  setBasename - assigns the basename with name.  The fragments
    #                will be named (basename % no).html
    #
    def setBasename (self, name):
        self.basename = name
    #
    #  setNodename - determines whether file is generated from the node
    #                name.
    #
    def setNodename (self, useNode):
        self.nodename = useNode
    #
    #  setFilename - assigns the filename to name providing that nodename
    #                is true.
    #
    def setFilename (self, name):
        if self.nodename:
            self.currentFilename = name
    #
    #  collectFragment - return the old fragment filename and string and
    #                    create a new filename and an empty new fragment.
    #
    def collectFragment (self, dataType):
        if dataType == "menu":
            f, s = "", self.currentFragment
            self.currentFragment = ""
        else:
            f, s = self.currentFilename, self.currentFragment
            self.currentFragment = ""
            self.maxFrag += 1
            sys.stderr.write('[%d] ' % self.maxFrag)
            if (self.maxFrag % 10) == 0:
                sys.stderr.write('\n')
            self.previousFilename = self.currentFilename
            if not self.nodename:
                self.currentFilename = self.basename % self.maxFrag
            self.anchorCount = 0
        return f, s
    #
    #  encodeChar -
    #
    def _encodeChar (self, c):
        global char2code
        if c in char2code:
            self.raw(char2code[c])
        else:
            self.raw(c)
    #
    #  puts - append, s, to the currentFragment, encoding each
    #         character in turn.
    #
    def puts (self, s):
        for i in s:
            self._encodeChar(i)
    #
    #  raw - append, s, to the currentFragment.
    #
    def raw (self, s):
        if self.output == None:
            self.currentFragment += s
        else:
            self.output.write(s)
    #
    #  openFragment - open up the initial fragment.
    #
    def openFragment (self):
        self.fragNo = 0
    #
    #  nextFragment - closes the last fragment and opens up the next
    #                 filename.
    #
    def nextFragment (self, name):
        if self.fragNo > 0:
            self.closeFragment()
        if config.multipleFragments:
            completePath = os.path.join (self.subDirectory, name)
            self.output = open (completePath, 'w')
        if config.debugFragments:
            print()
            print("----------------------------------------------")
            print(name)
            print("----------------------------------------------")
            print()
            self.output = sys.stdout
        self.fragNo += 1
        self.deviceHeader()
    #
    #  closeFragment - closes the final file fragment.
    #
    def closeFragment (self):
        self.deviceFooter()
        if config.multipleFragments:
            if self.output == None:
                sys.stderr.write ('output channel is None')
            else:
                self.output.close()
    #
    #  copyright - output the (C) symbol.
    #
    def copyright (self):
        self.raw('&copy;')
    #
    #  emitTagName - write out tag names
    #
    def _emitTagName(self, s):
        if (self.state == init_state) or (self.tag == no_tag):
            return
        self.raw(s)
        if self.tag == title_tag:
            self.raw('title>')
        elif self.tag == header1_tag:
            self.raw('h1>')
        elif self.tag == header2_tag:
            self.raw('h2>')
        elif self.tag == header3_tag:
            self.raw('h3>')
        elif self.tag == header4_tag:
            self.raw('h4>')
        elif self.tag == center_tag:
            self.raw('center>')
        elif self.tag == paragraph_tag:
            self.raw('p>')
        elif self.tag == preformatted_tag:
            self.raw('pre>')
        elif self.tag == teletype_tag:
            self.raw('tt>')
        elif self.tag == italic_tag:
            self.raw('i>')
        elif self.tag == bold_tag:
            self.raw('b>')

    #
    #  emitTag - write out the start tag
    #
    def _emitTag(self):
        self._emitTagName('<')
    #
    #  emitTagEnd - write out the end tag
    #
    def _emitTagEnd(self):
        if self.state in [copy_state, white_state, nf_state]:
            self._emitTagName('</')
            if self.tag in [header1_tag, header2_tag, header3_tag, header4_tag, title_tag, center_tag, paragraph_tag, preformatted_tag]:
                self.raw('\n')
            if not (self.tag in [teletype_tag, italic_tag, bold_tag]):
                self.raw('\n')
    def _initState (self):
        self.state = init_state
    def __init__ (self, name):
        self.state = init_state
        self.tag = null_tag
        self.stack = []
        self.lastnewline = False
        self.filenameList = []
        self.currentFragment = ""
        self.currentFilename = name
        self.previousFilename = name
        self.title = ""
        self.output = None
        self.fragNo = 0
        self.maxFrag = 0
        sys.stderr.write('[1] ')
        self.pendingDiv = False
        self.actualDiv = False
        self.anchorCount = 0
        self.templatePath = '.'
        self.nodename = False
        self.sectionCount = 0
        self.subDirectory = "."
    def _to (self, s):
        self.state = s
    def _status (self):
        return self.state
    def push (self):
        self.stack += [(self.state, self.tag)]
    def pop (self):
        self.state, self.tag = self.stack[-1]
        self.stack = self.stack[:-1]
        if self.state == white_state:
            self.state = copy_state
    def _twoNewlines (self, c):
        if (c == '\n') and (self.lastnewline) and (self.tag == paragraph_tag):
            self._end()
            self.paraBegin()
            return True
        else:
            self.lastnewline = (c == '\n')
            return False
    def _doChar (self, c):
        if self.state == known_state:
            if c in string.whitespace:
                return
            self._emitTag()
            self.puts(c)
            self.state = copy_state
        elif self.state == copy_state:
            if self._twoNewlines(c):
                return
            self.puts(c)
            if c in string.whitespace:
                self.state = white_state
        elif self.state == white_state:
            if self._twoNewlines(c):
                return
            if not (c in string.whitespace):
                self.puts(c)
                self.state = copy_state
        elif self.state == nf_state:
            self.puts(c)
    def _end (self):
        if (self.state == init_state) or (self.state == known_state):
            return
        self._emitTagEnd()
        self.lastnewline = False
        self.state = init_state
    def write (self, contents):
        if self.state == init_state:
            return
        for c in contents:
            self._doChar(c)
    def setTitle(self, content):
        title = content
    def titleBegin(self):
        self._end()
        self.state = known_state
        self.tag = title_tag
    def titleEnd(self):
        self._end()
    def paraBegin(self):
        self._end()
        self.state = known_state
        self.tag = paragraph_tag
    def paraEnd(self):
        self._end()
    def centerBegin(self):
        self._end()
        self.state = known_state
        self.tag = center_tag
    def centerEnd(self):
        self._end()
    def h1Begin(self):
        self._end()
        self.state = known_state
        self.tag = header1_tag
        self.sectionCount += 1
    def h1End(self):
        self._end()
    def h2Begin(self):
        self._end()
        self.state = known_state
        self.tag = header2_tag
        self.sectionCount += 1
    def h2End(self):
        self._end()
    def h3Begin(self):
        self._end()
        self.state = known_state
        self.tag = header3_tag
        self.sectionCount += 1
    def h3End(self):
        self._end()
    def h4Begin(self):
        self._end()
        self.state = known_state
        self.tag = header4_tag
        self.sectionCount += 1
    def h4End(self):
        self._end()
    def preBegin(self):
        self._end()
        self.state = nf_state
        self.tag = preformatted_tag
        self._emitTag()
    def preEnd(self):
        self._end()
    def ttBegin(self):
        self.push()
        self.state = nf_state
        self.tag = teletype_tag
        self._emitTag()
    def ttEnd(self):
        self._end()
        self.pop()
    def iBegin(self):
        self.push()
        self.tag = italic_tag
        self._emitTag()
    def iEnd(self):
        self._end()
        self.pop()
    def bBegin(self):
        self.push()
        self.tag = bold_tag
        self._emitTag()
    def bEnd(self):
        self._end()
        self.pop()
    def centerBegin(self):
        self._end()
        self.state = known_state
        self.tag = center_tag
    def end(self):
        self._end()
        self.state = init_state
        self.tag = null_tag
    def noBegin(self):
        self.push()
        self.state = known_state
        self.tag = no_tag
        self._emitTag()
    def noEnd(self):
        self._end()
        self.pop()
    #
    #  tableBegin -
    #
    def tableBegin(self):
        self.raw('<div class="itemize">\n')
        self.raw("<dl>\n")
        self.firstItem = True
        self.tableRightOpen = False
    #
    #  tableEnd -
    #
    def tableEnd(self):
        self.raw("</dl>\n")
        self.raw('</div>\n')
    #
    #  tableLeftBegin -
    #
    def tableLeftBegin(self):
        if self.firstItem:
            self.tableRightEnd()
        self.raw("<dt>")
        self.noBegin()
    #
    #  tableLeftEnd -
    #
    def tableLeftEnd(self):
        self.noEnd()
        self.firstItem = False
        self.raw("</dt>\n")
    #
    #  tableRightBegin -
    #
    def tableRightBegin(self):
        self._end()
        self.tableRightOpen = True
        self.raw("<dd>")
    #
    #  tableRightEnd -
    #
    def tableRightEnd(self):
        self._end()
        if self.tableRightOpen:
            self.raw("</dd>")
            self.tableRightOpen = False
    #
    #  itemizeBegin
    #
    def itemizeBegin(self):
        self._end()
        self.raw("<ul type=disc>")
    #
    #  itemizeEnd
    #
    def itemizeEnd(self):
        self._end()
        self.raw("</ul>")
    #
    #  item
    #
    def item(self):
        self._end()
        self.raw("<li>")
    #
    #  emitTitle
    #
    def emitTitle (self):
        self.titleBegin()
        self.write(title)
        self.titleEnd()
        if config.ignoreh1Titlepage:
            pass
        else:
            self.h1Begin()
            self.write(self.title)
            self.h1End()

    #
    #  deviceHeader - emit html header.
    #
    def deviceHeader (self):
        self.raw(self._safeOpen('header.ht', 'header template "header.ht"'))
    #
    #  deviceFooter - emit html footer.
    #
    def deviceFooter (self):
        self.raw(self._safeOpen('footer.ht', 'footer template "footer.ht"'))
    #
    #  safeOpen -
    #
    def _safeOpen (self, filename, description):
        try:
            for i in self.templatePath.split(':'):
                f = os.path.join(i, filename)
                if os.path.exists(f) and os.path.isfile(f):
                    return open(f, 'r').read()
            print(("cannot open", description))
            sys.exit(0)
        except:
            print(("cannot open", description))
            sys.exit(0)
    #
    #  emitMenuTitle -
    #
    def emitMenuTitle (self):
        self.raw(self._safeOpen('title.ht', 'title template "title.ht"'))
    #
    #  emitNodeHeading -
    #
    def emitNodeHeading (self):
        self.raw(self._safeOpen('heading.ht', 'heading template "heading.ht"'))
    #
    #
    #
    def getSectionAnchor (self):
        return "section%d" % self.sectionCount
    #
    #  sectionAnchor -
    #
    def sectionAnchor (self, content):
        self.raw('<a name="' + self.getSectionAnchor() + '"></a>\n')
        return self.currentFilename + '#' + self.getSectionAnchor()
