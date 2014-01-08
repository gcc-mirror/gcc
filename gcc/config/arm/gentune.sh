#!/bin/sh
# Generate arm-tune.md, a file containing the tune attribute from the list of 
# CPUs in arm-cores.def
# Copyright (C) 2004-2014 Free Software Foundation, Inc.
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

echo ";; -*- buffer-read-only: t -*-"
echo ";; Generated automatically by gentune.sh from arm-cores.def"

allcores=`awk -F'[(, 	]+' '/^ARM_CORE/ { cores = cores$3"," } END { print cores } ' $1`

echo "(define_attr \"tune\""
echo "	\"$allcores\"" | sed -e 's/,"$/"/' | sed -e 's/\([a-z0-9_]\+,[a-z0-9_]\+,[a-z0-9_]\+,\)/\1\n\t/g'
echo "	(const (symbol_ref \"((enum attr_tune) arm_tune)\")))"
