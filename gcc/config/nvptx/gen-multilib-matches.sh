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


nvptx_dir=$(dirname "$0")


nvptx_sm_def="$nvptx_dir/nvptx-sm.def"

sms=$(grep ^NVPTX_SM $nvptx_sm_def | sed 's/.*(//;s/,.*//')


# Every variant in 'sms' has to either be remapped to the default variant
# ('.', which is always built), or does get built as non-default variant
# ('misa=sm_SM'; thus not remapped), or has to be remapped to the "next lower"
# variant that does get built.

print_multilib_matches() {
    local sms
    sms=${1?}
    shift
    local multilib_options_isa_default
    multilib_options_isa_default=${1?}
    shift
    local multilib_options_isa_list
    multilib_options_isa_list=${1?}
    shift
    [ $# = 0 ]

    local multilib_matches
    multilib_matches=

    local sm_next_lower
    unset sm_next_lower

    local sm
    for sm in $sms; do
	local sm_map
	unset sm_map
	if [ x"sm_$sm" = x"$multilib_options_isa_default" ]; then
	    sm_map=.
	elif expr " $multilib_options_isa_list " : ".* sm_$sm " > /dev/null; then
	    sm_map=
	else
	    # Assert here that a "next lower" variant is available; the
	    # "lowest" variant always does get built.
	    sm_map=${sm_next_lower?}
	fi

	if [ x"${sm_map?}" = x ]; then
	    sm_next_lower=$sm
	else
	    local multilib_matches_sm
	    unset multilib_matches_sm
	    # Output format as required for 'MULTILIB_MATCHES'.
	    if [ x"$sm_map" = x. ]; then
		multilib_matches_sm=".=misa?sm_$sm"
	    else
		multilib_matches_sm="misa?sm_$sm_map=misa?sm_$sm"
	    fi
	    multilib_matches="$multilib_matches ${multilib_matches_sm?}"

	    sm_next_lower=$sm_map
	fi
    done

    echo "$multilib_matches"
}

multilib_matches=$(print_multilib_matches "$sms" "$@")
echo "multilib_matches := $multilib_matches"
