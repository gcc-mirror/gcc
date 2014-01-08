#!/bin/sh
# Generate rs6000-tables.opt from the list of CPUs in rs6000-cpus.def.
# Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

cat <<EOF
; -*- buffer-read-only: t -*-
; Generated automatically by genopt.sh from rs6000-cpus.def.

; Copyright (C) 2011-2014 Free Software Foundation, Inc.
;
; This file is part of GCC.
;
; GCC is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free
; Software Foundation; either version 3, or (at your option) any later
; version.
;
; GCC is distributed in the hope that it will be useful, but WITHOUT ANY
; WARRANTY; without even the implied warranty of MERCHANTABILITY or
; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
; for more details.
;
; You should have received a copy of the GNU General Public License
; along with GCC; see the file COPYING3.  If not see
; <http://www.gnu.org/licenses/>.

Enum
Name(rs6000_cpu_opt_value) Type(int)
Known CPUs (for use with the -mcpu= and -mtune= options):

EnumValue
Enum(rs6000_cpu_opt_value) String(native) Value(RS6000_CPU_OPTION_NATIVE) DriverOnly

EOF

awk -F'[(, 	]+' '
BEGIN {
    value = 0
}

/^RS6000_CPU/ {
    name = $2
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(rs6000_cpu_opt_value) String(" name ") Value(" value ")"
    print ""
    value++
}' $1/rs6000-cpus.def
