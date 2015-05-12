#!/bin/sh

# Checks some of the GNU style formatting rules in a set of patches.
# Copyright (C) 2010, 2012  Free Software Foundation, Inc.
# Contributed by Sebastian Pop <sebastian.pop@amd.com>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

usage() {
    cat <<EOF
check_GNU_style.sh [patch]...

    Checks the patches for some of the GNU style formatting problems.
    When FILE is -, read standard input.

    Please note that these checks are not always accurate, and
    complete.  The reference documentation of the GNU Coding Standards
    can be found here: http://www.gnu.org/prep/standards_toc.html
    and there are also some additional coding conventions for GCC:
    http://gcc.gnu.org/codingconventions.html

EOF
    exit 1
}

test $# -eq 0 && usage
nfiles=$#
files="$*"

stdin=false
stdin_tmp=""
if [ $nfiles -eq 1 ] && [ "$files" = "-" ]; then
    stdin=true

    # By putting stdin into a temp file, we can handle it just like any other
    # file.  F.i., we can cat it twice, which we can't do with stdin.
    stdin_tmp=check_GNU_style.stdin
    cat - > $stdin_tmp
    files=$stdin_tmp
else
    for f in $files; do
	if [ "$f" = "-" ]; then
	    # Let's keep things simple.  Either we read from stdin, or we read
	    # from files specified on the command line, not both.
	    usage
	fi
	if [ ! -f "$f" ]; then
	    echo "error: could not read file: $f"
	    exit 1
	fi
    done
fi

inp=check_GNU_style.inp
tmp=check_GNU_style.tmp

# Remove $tmp on exit and various signals.
trap "rm -f $inp $tmp $stdin_tmp" 0
trap "rm -f $inp $tmp $stdin_tmp; exit 1" 1 2 3 5 9 13 15

if [ $nfiles -eq 1 ]; then
    # There's no need for the file prefix if we're dealing only with one file.
    format="-n"
else
    format="-nH"
fi
grep $format '^+' $files \
    | grep -v ':+++' \
    > $inp

# Grep
g (){
    msg="$1"
    arg="$2"
    cat $inp \
	| egrep --color=always -- "$arg" \
	> $tmp && printf "\n$msg\n"
    cat $tmp
}

# And Grep
ag (){
    msg="$1"
    arg1="$2"
    arg2="$3"
    cat $inp \
	| egrep --color=always -- "$arg1" \
	| egrep --color=always -- "$arg2" \
	> $tmp && printf "\n$msg\n"
    cat $tmp
}

# reVerse Grep
vg (){
    msg="$1"
    varg="$2"
    arg="$3"
    cat $inp \
	| egrep -v -- "$varg" \
	| egrep --color=always -- "$arg" \
	> $tmp && printf "\n$msg\n"
    cat $tmp
}

col (){
    msg="$1"
    local first=true
    local f
    for f in $files; do
	local prefix=""
	if [ $nfiles -ne 1 ]; then
	    prefix="$f:"
	fi

	# Don't reuse $inp, which may be generated using -H and thus contain a
	# file prefix.
	grep -n '^+' $f \
	    | grep -v ':+++' \
	    > $tmp

	cat $tmp | while IFS= read -r line; do
	    local longline
	    # Filter out the line number prefix and the patch line modifier '+'
	    # to obtain the bare line, before we use expand.
	    longline=$(echo "$line" \
		| sed 's/^[0-9]*:+//' \
		| expand \
		| awk '{ if (length($0) > 80) print $0}')
	    if [ "$longline" != "" ]; then
		if $first; then
		    printf "\n$msg\n"
		    first=false
		fi
		echo "$prefix$line"
	    fi
	done
    done
}

col 'Lines should not exceed 80 characters.'

g 'Blocks of 8 spaces should be replaced with tabs.' \
    ' {8}'

g 'Trailing whitespace.' \
    '[[:space:]]$'

g 'Space before dot.' \
    '[[:alnum:]][[:blank:]]+\.'

g 'Dot, space, space, new sentence.' \
    '[[:alnum:]]\.([[:blank:]]|[[:blank:]]{3,})[A-Z0-9]'

g 'Dot, space, space, end of comment.' \
    '[[:alnum:]]\.([[:blank:]]{0,1}|[[:blank:]]{3,})\*/'

g 'Sentences should end with a dot.  Dot, space, space, end of the comment.' \
    '[[:alnum:]][[:blank:]]*\*/'

vg 'There should be exactly one space between function name and parentheses.' \
    '\#define' '[[:alnum:]]([[:blank:]]{2,})?\('

g 'There should be no space before closing parentheses.' \
    '[[:graph:]][[:blank:]]+\)'

ag 'Braces should be on a separate line.' \
    '\{' 'if[[:blank:]]\(|while[[:blank:]]\(|switch[[:blank:]]\('

