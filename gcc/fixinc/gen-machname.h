#! /bin/sh

# Copyright (C) 2000 Free Software Foundation, Inc.
# This file is part of GNU CC.

# GNU CC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# GNU CC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU CC; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# This script extracts from the specs file all the predefined macros
# that are not in the C89 reserved namespace (the reserved namespace
# is all identifiers beginnning with two underscores or one underscore
# followed by a capital letter).  The specs file is on standard input.
# A #define for a regular expression to find any of those macros in a
# header file is written to standard output.

# Note dependency on ASCII. \040 = space, \011 = tab, \012 = newline.
# tr ' ' '\n' is, alas, not portable.

tr -s '\040\011' '\012\012' |
    sed -n 's/^.*-D\([a-zA-Z_][a-zA-Z0-9_]*\).*$/\1/p' |
    sort -u > mn.T

if grep -v '^_[_A-Z]' mn.T > mn.U
then
    echo "Forbidden identifiers: `tr '\012' ' ' <mn.U`" >&2
    sed 's/^/\\\\</; s/$/\\\\>/' <mn.U | tr '\012' '|' > mn.V
    echo '' >>mn.V
    sed 's/^/#define MN_NAME_PAT "/; s/|$/"/' < mn.V
else
    echo "No forbidden identifiers defined by this target" >&2
    echo '#define MN_NAME_PAT ""'
fi
rm -f mn.[TUV]
exit 0
