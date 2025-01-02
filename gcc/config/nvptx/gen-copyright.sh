#!/bin/sh

# Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

style="$1"
case $style in
    opt)
    ;;
    c)
	first=true
    ;;
    *)
	echo "Unknown style: \"$style\""
	exit 1
	;;
esac

( cat <<EOF
Copyright (C) 2022-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.
EOF
) | while read line; do
    case $style in
	opt)
	    if [ "$line" = "" ]; then
		echo ";"
	    else
		echo "; $line"
	    fi
	    ;;
	c)
	    if $first; then
		echo "/* $line"
		first=false
	    else
		if [ "$line" = "" ]; then
		    echo
		else
		    echo "   $line"
		fi
	    fi
	    ;;
    esac
done


case $style in
    c)
	echo "*/"
	;;
esac
