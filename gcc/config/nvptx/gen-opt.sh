#!/bin/sh

# Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

nvptx_sm_def="$1/nvptx-sm.def"
gen_copyright_sh="$1/gen-copyright.sh"

sms=$(grep ^NVPTX_SM $nvptx_sm_def | sed 's/.*(//;s/,.*//')

last=
for sm in $sms; do
    last="$sm"
done

cat <<EOF
; -*- buffer-read-only: t -*-
; Generated automatically by gen-opt.sh from nvptx-sm.def.
EOF

# Separator.
echo

. $gen_copyright_sh opt

# Separator.
echo

cat <<EOF
Enum
Name(ptx_isa) Type(int)
Known PTX ISA target architectures (for use with the -misa= option):
EOF

# Separator.
echo

for sm in $sms; do
    cat <<EOF
EnumValue
Enum(ptx_isa) String(sm_$sm) Value(PTX_ISA_SM$sm)
EOF

    if [ "$sm" = "$last" ]; then
	# Don't end with trailing empty line.
	continue
    fi

    # Separator.
    echo
done
