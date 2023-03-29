#!/bin/sh
# Generate csky_tables.opt from the lists in *.def.
# Copyright (C) 2018-2023 Free Software Foundation, Inc.
# Contributed by C-SKY Microsystems and Mentor Graphics.
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
; Generated automatically by csky_genopt.sh from csky_cores.def.

; Copyright (C) 2018-2023 Free Software Foundation, Inc.
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
Name(csky_processor_type) Type(enum csky_processor_type)
Known CSKY CPUs (for use with the -mcpu= options):

EOF

awk -F'[(, 	]+' '/^CSKY_CORE/ {
    name = $2
    enum = $3
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(csky_processor_type) String(" name ") Value( TARGET_CPU_" enum ")"
    print ""
}' $1/csky_cores.def

cat <<EOF
Enum
Name(csky_arch) Type(int)
Known CSKY architectures (for use with the -march= option):

EOF

awk -F'[(, 	]+' 'BEGIN {
    value = 0
}
/^CSKY_ARCH/ {
    name = $2
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(csky_arch) String(" name ") Value(" value ")"
    print ""
    value++
}' $1/csky_cores.def

cat <<EOF
Enum
Name(csky_fpu) Type(enum csky_fpu_type)
Known CSKY FPUs (for use with the -mfpu= option):

EOF

awk -F'[(, 	]+' '
/^CSKY_FPU/ {
    name = $2
    enum = $3
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(csky_fpu) String(" name ") Value(TARGET_FPU_" enum ")"
    print ""
}
END {
    print "EnumValue"
    print "Enum(csky_fpu) String(auto) Value(TARGET_FPU_auto)"
}' $1/csky_cores.def
