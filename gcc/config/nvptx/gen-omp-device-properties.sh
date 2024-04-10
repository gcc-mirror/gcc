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

sms=$(grep ^NVPTX_SM $nvptx_sm_def | sed 's/.*(//;s/,.*//')

echo kind: gpu
echo arch: nvptx nvptx64

isa=""
for sm in $sms; do
    isa="$isa sm_$sm"
done

echo isa: $isa
