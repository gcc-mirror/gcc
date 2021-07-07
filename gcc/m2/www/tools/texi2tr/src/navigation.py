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

anchors = {}
nodes = {}

maxNoOfShortLabels = 10    # only allow this many different short tabs
maxLabelLength     = 90    # total character length of all menus

class nodeInfo:
    #
    #  record the texinfo nodes
    #
    def __init__ (self, me, n, p, u):
        self.name     = me
        self.__next__ = n
        self.prev     = p
        self.up       = u
    def selectTab (self, root, selected):
        if (root != None) and (root._isShort()):
            for i in root.list:
                if (i[0] == self.name) or (selected == ""):
                    return i[0]
        return selected
    def generateMenu (self, html, root, selected):
        if self.name == "Top":
            return selected
        html.emitMenuTitle()
        html.emitNodeHeading()
        self.emitPrevNext(html)
        selected = self.selectTab(root, selected)
        root.emitTab(html, selected)
        html.raw('</div>\n')
        return selected
    def requiresNewFile(self):
        return self.name != "Top"
    def _isShort (self):
        return False
    def _genShort (self, html, active):
        pass
    def emitTab (self, root, selected):
        pass
    def isNode (self):
        return True
    def getName (self):
        return self.name
    def emitPrevNext (self, html):
        global anchors

        html.raw("""
    <div id="navigation">
      <table width="100%" cellpadding="2" cellspacing="2">
	<tr valign="middle">
""")
        if (self.prev != "") and (self.prev != "Top") and self.prev in anchors:
            html.raw('''
	  <td><a accesskey="p" href="''')
            html.raw(anchors[self.prev])
            html.raw('"><img width="48" alt="Prev" src="prev.png" border="0" height="48"></img></a></td>')
        if (self.__next__ != "") and (self.__next__ != "Top") and self.__next__ in anchors:
            html.raw('''
	  <td align="right"><a accesskey="n" href="''')
            html.raw(anchors[self.__next__])
            html.raw('"><img width="48" alt="Next" src="next.png" border="0" height="48"></img></a></td>')
        html.raw('</tr></table></div>')


#
#  addNode - adds a node to the dictionary.
#

def addNode (html, line):
    global nodes

    w = line.split(',')
    if len(w)>4:
        w = w[:4]
    while len(w)<4:
        w += [""]
    me, n, p, u = w

    me = me.rstrip().lstrip()
    n = n.rstrip().lstrip()
    p = p.rstrip().lstrip()
    u = u.rstrip().lstrip()

    nodes[me] = nodeInfo(me, n, p, u)
    anchor(html, me)
    return nodes[me]

class menuInfo:
    #
    #  records the data contained in the menu texinfo sections
    #
    def __init__ (self, initial):
        self.list = []
        self.short = initial
    def isNode (self):
        return False
    #
    #  parseMenu - parse the content and populate the data structures.
    #
    def parseMenu (self, content):
        for line in content.split('\n'):
            l = line.lstrip().rstrip()
            if (len(l)>1) and (l[0] == '*'):
                l = l[1:]
                l = l.lstrip()
                i = l.find('::')
                if (i>0) and (i+2<len(l)):
                    m = l[i+2:].lstrip()
                    self.list += [[l[:i], m]]
    #
    #  debugMenu - dump the data structures
    #
    def debugMenu (self):
        for m in self.list:
            print((m[0], m[1]))
    #
    #  generateMenu - issues the menu
    #
    def generateMenu (self, html, root, selected):
        if self != root:
            self._genLong(html)
        return selected
    #
    #  requiresNewFile - returns False for a menu
    #
    def requiresNewFile(self):
        return False
    #
    #
    #
    def emitTab (self, html, selected):
        if self._isShort():
            self._genShort(html, selected)
    #
    #  isShort - return True if the menu can be encoded in a short form.
    #
    def _isShort (self):
        global maxNoOfShortLabels, maxLabelLength

        if (len(self.list)>maxNoOfShortLabels) or (not self.short):
            return False
        t = 0
        for m in self.list:
            t += len(m[0])
        return t<maxLabelLength
    #
    #  genShort - generate a list of tabs for the short menu
    #
    def _genShort (self, html, active):
        html.raw('<div id="tabmenu">\n')
        html.raw('<ul id="tab">\n')
        for m in self.list:
            if m[0] in anchors:
                active = litab(html, anchors[m[0]], m[0], active)
            else:
                if (len(m[1]) > 1) and (m[1][-1] == '.') and (m[1][:-1] in anchors):
                    active = litab(html, anchors[m[1][:-1]], m[0], active)
                elif m[1] in anchors:
                    active = litab(html, anchors[m[1]], m[0], active)
                else:
                    print(("cannot find anchor for section", m[0], "or", m[1]))
        html.raw('\n</ul>\n')
        html.raw('</div>\n')
    #
    #  genLong - generate a unordered list for the short menu
    #
    def _genLong (self, html):
        html.raw('\n<ul>\n')
        for m in self.list:
            if m[0] in anchors:
                liurl(html, anchors[m[0]], m[1])
            else:
                if (len(m[1]) > 1) and (m[1][-1] == '.') and (m[1][:-1] in anchors):
                    liurl(html, anchors[m[1][:-1]], m[1])
                elif m[1] in anchors:
                    liurl(html, anchors[m[1]], m[1])
                else:
                    print(("cannot find anchor for section", m[0], "or", m[1]))
        html.raw('</ul>\n')

#
#  anchor - adds an anchor to the output and enters it into our dictionary
#

def anchor (html, label):
    global anchors

    if label in anchors:
        print(("node", label, "already exists"))
    anchors[label] = html.getNodeLink()
    s = '<a name="' + html.getNodeAnchor() + '"></a>\n'
    html.raw(s)
    html.nextAnchor()

#
#  liurl - issue a <li> url </li> as long as text is not empty.
#

def liurl (html, link, text):
    if text != "":
        html.raw('<li>\n')
        url(html, link, text)
        html.raw('</li>\n')

#
#  url - issue a link providing that text is non nul.
#

def url (html, link, text):
    if text != "":
        html.raw('<a href="')
        html.raw(link)
        html.raw('">')
        html.puts(text)
        html.raw('</a>')

#
#  litab - issue a tab link providing that text is non nul.
#

def litab (html, link, text, selected):
    if text != "":
        if (selected == ""):
            html.raw('<li class="selected">')
            selected = text
        elif selected == text:
            html.raw('<li class="selected">')
        else:
            html.raw('<li>')
        html.raw('<a href="')
        html.raw(link)
        html.raw('"><span>')
        html.puts(text)
        html.raw('</span></a></li>\n')
        return selected

#
#  getRoot - returns the root menu
#

def getRoot (frags):
    for m in frags:
        if (m[0] == 'menu') or (m[0] == 'node'):
            if (not m[1].isNode()) and (m[1]._isShort()):
                return m[1]
    return None
