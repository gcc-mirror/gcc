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


nvptx_dir=$(dirname "$0")


nvptx_sm_def="$nvptx_dir/nvptx-sm.def"
gen_copyright_sh="$nvptx_dir/gen-copyright.sh"

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

# Not emitting the following here (in addition to having it in 'nvptx.opt'), as
# we'll otherwise run into:
# 
#     gtyp-input.list:10: file [...]/gcc/config/nvptx/nvptx-opts.h specified more than once for language (all)
#     make[2]: *** [Makefile:2981: s-gtype] Error 1
: ||
cat <<EOF

HeaderInclude
config/nvptx/nvptx-opts.h
EOF

# Separator.
echo

cat <<EOF
Enum
Name(ptx_isa) Type(enum ptx_isa)
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
