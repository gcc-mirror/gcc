#! /bin/sh
# Generate c6x-sched.md from c6x-sched.md.in
# The input file is passed as an argument.

# Copyright (C) 2010-2020 Free Software Foundation, Inc.

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 3, or (at your option)
#any later version.

#GCC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING3.  If not see
#<http://www.gnu.org/licenses/>.

echo ";; -*- buffer-read-only: t -*-"
echo ";; Generated automatically from c6x-sched.md.in by gensched.sh"

for cross in n y; do
    for side in 1 2; do
	tside=$side
	case $side in
	    1) rf="a"; otherside=2 ;;
	    2) rf="b"; otherside=1 ;;
	esac
	case $cross in
	    y) cunit="+x$side"; tside=$otherside;;
	    n) cunit="";;
	esac
	echo
	echo ";; Definitions for side $side, cross $cross"
	echo
	sed -e "s,_CROSS_,$cross,g" -e "s,_CUNIT_,$cunit,g" \
	    -e "s,_N_,$side,g" -e "s,_RF_,$rf,g" -e "s,_NX_,$tside,g" \
	    < $1
    done
done
