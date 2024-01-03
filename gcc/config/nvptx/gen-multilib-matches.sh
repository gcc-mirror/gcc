#!/bin/sh

# Print nvptx 'MULTILIB_MATCHES'

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

set -e

nvptx_sm_def="$1/nvptx-sm.def"
multilib_options_isa_default=$2
multilib_options_isa_list=$3

sms=$(grep ^NVPTX_SM $nvptx_sm_def | sed 's/.*(//;s/,.*//')

# Every variant in 'sms' has to either be remapped to the default variant
# ('.', which is always built), or does get built as non-default variant
# ('misa=sm_SM'; thus not remapped), or has to be remapped to the "next lower"
# variant that does get built.

# The "lowest" variant has to be built.
sm_next_lower=INVALID

for sm in $sms; do
    if [ x"sm_$sm" = x"$multilib_options_isa_default" ]; then
	sm_map=.
    elif expr " $multilib_options_isa_list " : ".* sm_$sm " > /dev/null; then
	sm_map=
    else
	sm_map=$sm_next_lower
    fi

    if [ x"$sm_map" = x ]; then
	sm_next_lower=$sm
    else
	# Output format as required for 'MULTILIB_MATCHES'.
	if [ x"$sm_map" = x. ]; then
	    echo ".=misa?sm_$sm"
	else
	    echo "misa?sm_$sm_map=misa?sm_$sm"
	fi

	sm_next_lower=$sm_map
    fi
done
