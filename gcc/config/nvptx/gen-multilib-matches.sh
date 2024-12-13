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
gen_multilib_matches_tests="$nvptx_dir/gen-multilib-matches-tests"

sms=$(grep ^NVPTX_SM $nvptx_sm_def | sed 's/.*(//;s/,.*//')


# Every variant in 'sms' has to either be remapped to the default variant
# ('.', which is always built), or does get built as non-default variant
# ('misa=sm_SM'; thus not remapped), or gets remapped to a suitable variant,
# typically the "next lower" one that does get built.  If no "next lower" one
# does get built, then remap to the "lowest" one that does get built.  This
# increases chances that the linked code is compatible with more GPU hardware
# (backward compatibility).  For example, for GCC built '--with-arch=sm_80',
# '--with-multilib-list=sm_53', only 'sm_53' and 'sm_80' target libraries get
# built.  If now requesting a '-march=[...]' where no corresponding or "next
# lower" variant of the target libraries have been built, GCC's default
# behavior is to link in the default variant, 'sm_80'.  However, if compiling
# user code with '-march=sm_35', for example, linking in the 'sm_53' variant is
# supposedly more useful in terms of compatibility with GPU hardware.

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

    # Determine the "lowest" variant that does get built.
    local sm_next_lower
    sm_next_lower=.
    local sm
    for sm in $sms; do
	if [ x"sm_$sm" = x"$multilib_options_isa_default" ]; then
	    continue
	elif expr " $multilib_options_isa_list " : ".* sm_$sm " > /dev/null; then
	    sm_next_lower=$sm
	    break
	fi
    done

    local sm
    for sm in $sms; do
	local sm_map
	unset sm_map
	if [ x"sm_$sm" = x"$multilib_options_isa_default" ]; then
	    sm_map=.
	elif expr " $multilib_options_isa_list " : ".* sm_$sm " > /dev/null; then
	    sm_map=
	else
	    sm_map=$sm_next_lower
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


selftest() {
    [ $# = 0 ]

    local sms_default
    sms_default=$sms

    local name
    unset name
    local sms
    unset sms
    local multilib_options_isa_default
    unset multilib_options_isa_default
    local multilib_options_isa_list
    unset multilib_options_isa_list
    local multilib_matches_expected
    unset multilib_matches_expected

    local line
    line=0
    local f1 f2
    unset f1 f2
    while read -r f1 f2; do
	line=$((line + 1))
	case "$f1 $f2" in
	    ' ' | '#'* )
		:
		;;
	    'BEGIN '* )
		name=$f2
		sms=$sms_default
		unset multilib_options_isa_default
		unset multilib_options_isa_list
		unset multilib_matches_expected
		;;
	    'SSMS '* )
		sms=$f2
		;;
	    'SMOID '* )
		multilib_options_isa_default=$f2
		;;
	    'SMOIL '* )
		multilib_options_isa_list=$f2
		;;
	    'AEMM '* )
		multilib_matches_expected="$multilib_matches_expected $f2"
		;;
	    'CMMC ' )
		local multilib_matches
		multilib_matches=$(print_multilib_matches "${sms?}" "${multilib_options_isa_default?}" "${multilib_options_isa_list?}")
		if [ "$multilib_matches" = "$multilib_matches_expected" ]; then
		    echo >&2 "$0": selftest PASS "${name?}" at "$gen_multilib_matches_tests:$line"
		else
		    echo >&2 "$0": selftest FAIL "${name?}" at "$gen_multilib_matches_tests:$line"
		    echo >&2 expected:"$multilib_matches_expected"
		    echo >&2 actual:"$multilib_matches"
		    exit 1
		fi
		;;
	    * )
		echo >&2 "$0": selftest ERROR at "$gen_multilib_matches_tests:$line"
		echo >&2 invalid directive: "$f1 $f2"
		exit 1
		;;
	esac
    done < "$gen_multilib_matches_tests"
}


case "${1?}" in
    --selftest )
	shift
	selftest "$@"
	:;;
    * )
	multilib_matches=$(print_multilib_matches "$sms" "$@")
	echo "multilib_matches := $multilib_matches"
	;;
esac
