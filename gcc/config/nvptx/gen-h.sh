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


nvptx_dir=$(dirname "$0")


nvptx_sm_def="$nvptx_dir/nvptx-sm.def"
gen_copyright_sh="$nvptx_dir/gen-copyright.sh"

sms=$(grep ^NVPTX_SM $nvptx_sm_def | sed 's/.*(//;s/,.*//')

cat <<EOF
/* -*- buffer-read-only: t -*-
   Generated automatically by gen-h.sh from nvptx-sm.def.
*/
EOF

# Separator.
echo

. $gen_copyright_sh c

# Separator.
echo

for sm in $sms; do
    cat <<EOF
#define TARGET_SM$sm (ptx_isa_option >= PTX_ISA_SM$sm)
EOF
done
