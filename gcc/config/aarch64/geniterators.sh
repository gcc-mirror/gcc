#!/bin/sh
#
# Copyright (C) 2014-2024 Free Software Foundation, Inc.
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
#
# Find the <ITERATOR> definitions (may span several lines).
LC_ALL=C awk '
BEGIN {
	print "/* -*- buffer-read-only: t -*- */"
	print "/* Generated automatically by geniterators.sh from iterators.md.  */"
	print "#ifndef GCC_AARCH64_ITERATORS_H"
	print "#define GCC_AARCH64_ITERATORS_H"
}

{
	sub(/;.*/, "")
}

iterdef {
	s = s " " $0
}

!iterdef && /\(define_mode_iterator/ {
	iterdef = 1
	s = $0
	sub(/.*\(define_mode_iterator/, "", s)
}

iterdef {
	# Count the parentheses, the iterator definition ends
	# if there are more closing ones than opening ones.
	nopen = gsub(/\(/, "(", s)
	nclose = gsub(/\)/, ")", s)
	if (nopen >= nclose)
		next

	iterdef = 0

	gsub(/[ \t]+/, " ", s)
	sub(/ *\)[^)]*$/, "", s)
	sub(/^ /, "", s)

	# Drop the conditions.
	gsub(/ *"[^"]*" *\)/, "", s)
	gsub(/\( */, "", s)

	if (s !~ /^[A-Za-z0-9_]+ \[[A-Za-z0-9 ]*\]$/)
		next
	sub(/\[ */, "", s)
	sub(/ *\]/, "", s)

	n = split(s, a)
	printf "#define BUILTIN_" a[1] "(T, N, MAP, FLAG) \\\n"
	printf "  VAR" (n-1) " (T, N, MAP, FLAG"
	for (i = 2; i <= n; i++)
		printf ", "  tolower(a[i])
	printf ")\n"
}

END {
	print "#endif /* GCC_AARCH64_ITERATORS_H  */"
}' $1
