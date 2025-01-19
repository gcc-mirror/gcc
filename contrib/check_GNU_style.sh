#!/bin/sh

# Checks some of the GNU style formatting rules in a set of patches.
# Copyright (C) 2010-2025 Free Software Foundation, Inc.
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
# along with this program; if not, see the file COPYING3.  If not,
# see <http://www.gnu.org/licenses/>.

# Set to empty in the environment to override.
: ${color:---color=always}

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
tmp2=check_GNU_style.2.tmp
tmp3=check_GNU_style.3.tmp

# Remove $tmp on exit and various signals.
trap "rm -f $inp $tmp $tmp2 $tmp3 $stdin_tmp" 0
trap "rm -f $inp $tmp $tmp2 $tmp3 $stdin_tmp; exit 1" 1 2 3 5 9 13 15

if [ $nfiles -eq 1 ]; then
    # There's no need for the file prefix if we're dealing only with one file.
    format="-n"
else
    format="-nH"
fi

# Remove the testsuite part of the diff.  We don't care about GNU style
# in testcases and the dg-* directives give too many false positives.
remove_testsuite ()
{
  awk 'BEGIN{testsuite=0} /^(.*:)?([1-9][0-9]*:)?\+\+\+ / && ! /testsuite\//{testsuite=0} \
       {if (!testsuite) print} /^(.*:)?([1-9][0-9]*:)?\+\+\+ (.*\/)?testsuite\//{testsuite=1}'
}

grep $format '^+' $files \
    | remove_testsuite \
    | grep -v ':+++' \
    > $inp

cat_with_prefix ()
{
    local f="$1"

    if [ "$prefix" = "" ]; then
	cat "$f"
    else
	awk "{printf \"%s%s\n\", \"$prefix\", \$0}" $f
    fi
}

# Grep
g (){
    local msg="$1"
    local arg="$2"

    local found=false
    cat $inp \
	| egrep $color -- "$arg" \
	> "$tmp" && found=true

    if $found; then
	printf "\n$msg\n"
	cat "$tmp"
    fi
}

# And Grep
ag (){
    local msg="$1"
    local arg1="$2"
    local arg2="$3"

    local found=false
    cat $inp \
	| egrep $color -- "$arg1" \
	| egrep $color -- "$arg2" \
	> "$tmp" && found=true

    if $found; then
	printf "\n$msg\n"
	cat "$tmp"
    fi
}

# reVerse Grep
vg (){
    local msg="$1"
    local varg="$2"
    local arg="$3"

    local found=false
    cat $inp \
	| egrep -v -- "$varg" \
	| egrep $color -- "$arg" \
	> "$tmp" && found=true

    if $found; then
	printf "\n$msg\n"
	cat "$tmp"
    fi
}

col (){
    local msg="$1"

    local first=true
    local f
    for f in $files; do
	prefix=""
	if [ $nfiles -ne 1 ]; then
	    prefix="$f:"
	fi

	# Don't reuse $inp, which may be generated using -H and thus contain a
	# file prefix.  Re-remove the testsuite since we're not using $inp.
	cat $f | remove_testsuite \
	    | grep -n '^+' \
	    | grep -v ':+++' \
	    > $tmp

	# Keep only line number prefix and patch modifier '+'.
	cat "$tmp" \
	    | sed 's/\(^[0-9][0-9]*:+\).*/\1/' \
	    > "$tmp2"

	# Remove line number prefix and patch modifier '+'.
	# Expand tabs to spaces according to tab positions.
	# Keep long lines, make short lines empty.  Print the part past 80 chars
	# in red.
	cat "$tmp" \
	    | sed 's/^[0-9]*:+//' \
	    | expand \
	    | awk '{ \
		     if (length($0) > 80) \
		       printf "%s\033[1;31m%s\033[0m\n", \
			      substr($0,1,80), \
			      substr($0,81); \
		     else \
		       print "" \
		   }' \
	    > "$tmp3"

	# Combine prefix back with long lines.
	# Filter out empty lines.
	local found=false
	paste -d '\0' "$tmp2" "$tmp3" \
	    | grep -v '^[0-9][0-9]*:+$' \
	    > "$tmp" && found=true

	if $found; then
	    if $first; then
		printf "\n$msg\n"
		first=false
	    fi
	    cat_with_prefix "$tmp"
	fi
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

vg 'There should be exactly one space between function name and parenthesis.' \
    '\#define' \
    '[[:alnum:]]([[:blank:]]{2,})?\('

g 'There should be no space before a left square bracket.' \
   '[[:alnum:]][[:blank:]]+\['

g 'There should be no space before closing parenthesis.' \
    '[[:graph:]][[:blank:]]+\)'

# This will give false positives for C99 compound literals.
g 'Braces should be on a separate line.' \
    '(\)|else)[[:blank:]]*{'

# Does this apply to definitions of aggregate objects?
ag 'Trailing operator.' \
  '^[1-9][0-9]*:\+[[:space:]]' \
  '(([^a-zA-Z_]\*)|([-%<=&|^?])|([^*]/)|([^:][+]))$'
