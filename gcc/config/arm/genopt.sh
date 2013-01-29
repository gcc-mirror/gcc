#!/bin/sh
# Generate arm-tables.opt from the lists in *.def.
# Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
; Generated automatically by genopt.sh from arm-cores.def, arm-arches.def
; and arm-fpus.def.

; Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
Name(processor_type) Type(enum processor_type)
Known ARM CPUs (for use with the -mcpu= and -mtune= options):

EOF

awk -F'[(, 	]+' '/^ARM_CORE/ {
    name = $2
    enum = $3
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(processor_type) String(" name ") Value(" enum ")"
    print ""
}' $1/arm-cores.def

cat <<EOF
Enum
Name(arm_arch) Type(int)
Known ARM architectures (for use with the -march= option):

EOF

awk -F'[(, 	]+' 'BEGIN {
    value = 0
}
/^ARM_ARCH/ {
    name = $2
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(arm_arch) String(" name ") Value(" value ")"
    print ""
    value++
}' $1/arm-arches.def

cat <<EOF
Enum
Name(arm_fpu) Type(int)
Known ARM FPUs (for use with the -mfpu= option):

EOF

awk -F'[(, 	]+' 'BEGIN {
    value = 0
}
/^ARM_FPU/ {
    name = $2
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(arm_fpu) String(" name ") Value(" value ")"
    print ""
    value++
}' $1/arm-fpus.def
