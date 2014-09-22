#!/bin/sh
#
# Copyright (C) 2014 Free Software Foundation, Inc.
# Contributed by ARM Ltd.
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
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Generate aarch64-builtin-iterators.h, a file containing a series of
# BUILTIN_<ITERATOR> macros, which expand to VAR<N> Macros covering the
# same set of modes as the iterator in iterators.md

echo "/* -*- buffer-read-only: t -*- */"
echo "/* Generated automatically by geniterators.sh from iterators.md.  */"
echo "#ifndef GCC_AARCH64_ITERATORS_H"
echo "#define GCC_AARCH64_ITERATORS_H"

# Strip newlines, create records marked ITERATOR, and strip junk (anything
# which does not have a matching brace because it contains characters we
# don't want to or can't handle (e.g P, PTR iterators change depending on
# Pmode and ptr_mode).
cat $1 | tr "\n" " " \
       | sed 's/(define_mode_iterator \([A-Za-z0-9_]*\) \([]\[A-Z0-9 \t]*\)/\n#define BUILTIN_\1(T, N, MAP) \\ \2\n/g' \
       | grep '#define [A-Z0-9_(), \\]* \[[A-Z0-9[:space:]]*]' \
       | sed 's/\t//g' \
       | sed 's/  \+/ /g' \
       | sed 's/ \[\([A-Z0-9 ]*\)]/\n\L\1/' \
       | awk ' BEGIN { FS = " " ; OFS = ", "} \
	       /#/ { print } \
               ! /#/ { $1 = $1 ; printf "  VAR%d (T, N, MAP, %s)\n", NF, $0 }'

echo "#endif /* GCC_AARCH64_ITERATORS_H  */"
