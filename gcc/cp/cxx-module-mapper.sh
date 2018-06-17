#!/bin/bash

# Copyright (C) 2018 Free Software Foundation, Inc.
# Written by Nathan Sidwell <nathan@acm.org> while at FaceBook

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 3, or (at your option)
#any later version.

#GCC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING3.  If not see
#<http://www.gnu.org/licenses/>.

# Eventually this should be a portable compiled program, like
# lto-wrapper.

VERSION=0
progname=${0##*/}
main=

verbose=false
declare -A mapping

shopt -s extglob nullglob

bmi () {
    if test ${#mapping[@]} -ne 0 ; then
	echo "${mapping[$1]}"
    else
	echo "$(echo $1 | tr . -).nms"
    fi
}

search () {
    echo "$(echo $1 | tr . -).cc"
}

# outstanding async requests
async_ix=0

cmd () {
    resp=
    case "$1" in
	(AWAIT)
	    # process an ASYNC
	    if test $async_ix -ne 0 ; then
		((async_ix--))
		eval cmd '"${'async$async_ix'[@]}"'
		eval resp='"OK ${'async$async_ix'[1]} $line"'
	    else
		resp="ERROR No async request"
	    fi
	    ;;
	(ASYNC)
    	    # Append ASYNC request
	    shift
	    eval async$async_ix='("$@")'
	    ((async_ix++))
	    resp=OK
	    ;;
	(BMI)
	    # Map to a bmi name
	    bmi=$(bmi $2)
	    if test -z "$bmi" ; then
		resp="ERROR Unknown module name"
	    else
		resp="BMI $bmi"
	    fi
	    ;;
	(EXPORT)
	    resp="BMI $(bmi $2)"
	    ;;
	(DONE)
	    resp="OK"
	    ;;
	(HELLO)
	    if test "$2" = $VERSION ; then
		main="$4"
		resp="HELLO $VERSION 0 ."
	    else
		resp="ERROR Bad version (expect $VERSION)"
	    fi
	    ;;
	(HELP)
	    case "$2" in
		(query) resp="BMI|SEARCH|PATH" ;;
		(ASYNC) resp="ASYNC query" ;;
		(AWAIT) resp="AWAIT" ;;
		(BMI) resp="BMI <module> [<from>]" ;;
		(DONE) resp="DONE <module>" ;;
		(EXPORT) resp="EXPORT <module>" ;;
		(HELLO) resp="HELLO <ver> <src>" ;;
		(INCLUDE) resp="INCLUDE <header> [<from>]" ;;
		(PATH) resp="PATH <module> [<from>]" ;;
		(SEARCH) resp="SEARCH <module> [<from>]" ;;
		(*) resp="HELP, HELLO, ASYNC, AWAIT, BMI, DONE, EXPORT, INCLUDE, PATH, RESET, SEARCH" ;;
	    esac
	    ;;
	(INCLUDE)
	    # We have no legacy header support.  Always include.
	    resp="INCLUDE"
	    ;;
	(PATH)
	    resp="ERROR Unimplemented"
	    ;;
	(RESET)
	    main=""
	    ;;
	(SEARCH)
	    resp="$(search $2)"
	    ;;
	(*)
	    echo "Unknown command '$1'" >&2
	    resp="ERROR Bad Request"
	    ;;
    esac
    line="$resp"
}

while test "$#" != 0 ; do
    case "$1" in
	(-v) verbose=true ;;
	(--no-compile) compile=false ;;
	(--mapping)
	    shift
	    while read mod file ;
	    do
		mapping[$mod]="$file"
	    done < $1
	    ;;
	(-*) echo "Unknown option '$1'" >&2 ; exit 1 ;;
	(*) break ;;
    esac
    shift
done

ix=0
is_batch=false
# module oracle -> modacle
while read -a args -p "modacle>" ; do
    $verbose && echo "$progname< ${args[@]}" >&2
    # read is 0-based
    first=${args[0]}
    # deal with batching
    more=false
    case "$first" in
	(+*) args[0]=${first#+}
	     is_batch=true
	     more=true ;;
	(-*) args[0]=${first#-}
	     ;;
	(*)  ;;
    esac
    eval args$ix='("${args[@]}")'
    if $more ; then
	((ix++))
    else
	# process the batch
	resp_ix=0
	done_async=false
	for jx in $(seq 0 $ix) ; do
	    again=true
	    while $again ; do
		again=false
		eval first='${'args$jx'[0]}'
		line=""
		append=false
		if test "$first" ; then
		    eval cmd '"${'args$jx'[@]}"'
		    $verbose && echo "$progname> $line" >&2
		    append=true
		fi
		if $is_batch ; then
		    case "$first" in
			("") append=false ;;
			(AWAIT)
			    if test $async_ix -ne 0 ; then
				again=true
			    fi
			    done_async=false
			    ;;
			(ASYNC)
			    if $done_async; then
				append=false
			    else
				done_async=true
			    fi
			    ;;
			(*) done_async=false ;;
		    esac
		fi
		if $append ; then
		    eval resp$resp_ix'="$line"'
		    ((resp_ix++))
		fi
	    done
	done
	if test "$resp_ix" -eq 1 ; then
	    echo "$resp0"
	else
	    eval resp$resp_ix=""
	    pfx=+
	    for jx in $(seq 0 $resp_ix) ; do
		if test $jx -eq $resp_ix ; then
		    pfx=-
		fi
		eval echo '"$pfx$'resp$jx'"'
	    done
	fi
	is_batch=false
	ix=0
    fi
done
