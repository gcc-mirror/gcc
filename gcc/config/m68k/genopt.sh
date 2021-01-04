#!/bin/sh
# Generate m68k-tables.opt from the lists in *.def.
# Copyright (C) 2011-2021 Free Software Foundation, Inc.
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
; Generated automatically by genopt.sh from m68k-devices.def,
; m68k-isas.def and m68k-microarchs.def.

; Copyright (C) 2011-2021 Free Software Foundation, Inc.
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
Name(target_device) Type(enum target_device)
Known M68K CPUs (for use with the -mcpu= option):

EOF

awk -F'[(, 	]+' '/^M68K_DEVICE/ {
    name = $2
    enum = $3
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(target_device) String(" name ") Value(" enum ")"
    print ""
}' $1/m68k-devices.def

cat <<EOF
Enum
Name(uarch_type) Type(enum uarch_type)
Known M68K microarchitectures (for use with the -mtune= option):

EOF

awk -F'[(, 	]+' '/^M68K_MICROARCH/ {
    name = $2
    enum = $4
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(uarch_type) String(" name ") Value(u" enum ")"
    print ""
}' $1/m68k-microarchs.def

cat <<EOF
Enum
Name(m68k_isa) Type(int)
Known M68K ISAs (for use with the -march= option):

EOF

awk -F'[(, 	]+' 'BEGIN {
    value = 0
}
/^M68K_ISA/ {
    name = $2
    gsub("\"", "", name)
    print "EnumValue"
    print "Enum(m68k_isa) String(" name ") Value(" value ")"
    print ""
    value++
}' $1/m68k-isas.def
