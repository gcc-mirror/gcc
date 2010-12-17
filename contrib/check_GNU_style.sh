#!/bin/sh

# Checks some of the GNU style formatting rules in a set of patches.
# Copyright (C) 2010  Free Software Foundation, Inc.
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
    Please note that these checks are not always accurate, and
    complete.  The reference documentation of the GNU Coding Standards
    can be found here: http://www.gnu.org/prep/standards_toc.html
    and there are also some additional coding conventions for GCC:
    http://gcc.gnu.org/codingconventions.html

EOF
    exit 1
}

test $# -eq 0 && usage

tmp=check_GNU_style.tmp

# Grep
g (){
    msg="$1"
    arg="$2"
    shift 2
    grep -nH '^+' $* \
	| grep -v ':+++' \
	| egrep --color=always -- "$arg" \
	> $tmp && printf "\n$msg\n"
    cat $tmp
}

# And Grep
ag (){
    msg="$1"
    arg1="$2"
    arg2="$3"
    shift 3
    grep -nH '^+' $* \
	| grep -v ':+++' \
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
    shift 3
    grep -nH '^+' $* \
	| grep -v ':+++' \
	| egrep -v -- "$varg" \
	| egrep --color=always -- "$arg" \
	> $tmp && printf "\n$msg\n"
    cat $tmp
}

col (){
    msg="$1"
    shift 1
    grep -nH '^+' $* \
	| grep -v ':+++' \
	| cut -f 2 -d '+' \
	| awk '{ if (length ($0) > 80) print $0 }' \
	> $tmp
    if [ -s $tmp ]; then
	printf "\n$msg\n"
	cat $tmp
    fi
}

col 'Lines should not exceed 80 characters.' $*

g 'Trailing whitespace.' \
    '[[:space:]]$' $*

g 'Space before dot.' \
    '[[:alnum:]][[:blank:]]+\.' $*

g 'Dot, space, space, new sentence.' \
    '[[:alnum:]]\.([[:blank:]]|[[:blank:]]{3,})[[:alnum:]]' $*

g 'Dot, space, space, end of comment.' \
    '[[:alnum:]]\.([[:blank:]]{0,1}|[[:blank:]]{3,})\*/' $*

g 'Sentences should end with a dot.  Dot, space, space, end of the comment.' \
    '[[:alnum:]][[:blank:]]*\*/' $*

vg 'There should be exactly one space between function name and parentheses.' \
    '\#define' '[[:alnum:]]([^[:blank:]]|[[:blank:]]{2,})\(' $*

g 'There should be no space before closing parentheses.' \
    '[[:graph:]][[:blank:]]+\)' $*

ag 'Braces should be on a separate line.' \
    '\{' 'if[[:blank:]]\(|while[[:blank:]]\(|switch[[:blank:]]\(' $*


