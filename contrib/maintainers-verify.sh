#!/bin/sh

# Copyright (C) 2018 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

if [ "$1" != "" ]; then
    f="$1"
else
    f=./MAINTAINERS
fi

grep @ $f \
    | sed 's/[\t][\t]*/\t/g' \
    | awk -F '\t' \
	  "
{
  if (NF == 2) {
    name=\$1
    email=\$2
    if (names[name] == 1) {
        printf \"Redundant in write approval: %s\n\", name
    }
  } else if (NF == 3 ) {
    name=\$2
    email=\$3
    names[name] = 1
  }
}
"
