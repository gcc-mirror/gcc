#!/bin/sh
# Copyright (C) 2024-2025 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# This file was written by Gaius Mulley (gaiusmod2@gmail.com)

# This script performs base language translation tests with the bootstrap tool mc.

MC=$1
GCCSRCDIR=$2
GCCBUILDDIR=$3
GXX=$4


SCRATCHDIR=${GCCBUILDDIR}/m2/mc-base-lang-test


function compile () {
    echo $*
    if ! $* ; then
	echo $*
	echo "bootstrap compiler ${MC} failed: $*"
	exit 1
    fi
}


function compile_def () {
    SRC=$1
    compile ${MC} -o=${SCRATCHDIR}/G${SRC}.h --olang=c++  --h-file-prefix=G --quiet \
	    -I${GCCSRCDIR}/m2/gm2-libs -I${GCCSRCDIR}/testsuite/gm2/base-lang/pass \
	    ${GCCSRCDIR}/testsuite/gm2/base-lang/pass/${SRC}.def
}


function compile_mod () {
    SRC=$1
    compile ${MC} -o=${SCRATCHDIR}/G${SRC}.cc --olang=c++  --h-file-prefix=G --quiet \
	    -I${GCCSRCDIR}/m2/gm2-libs -I${GCCSRCDIR}/testsuite/gm2/base-lang/pass \
	    ${GCCSRCDIR}/testsuite/gm2/base-lang/pass/${SRC}.mod
}


#
#  verify - check that the translated sources compile with -Wodr
#

function verify () {
    SRC=$1
    if ${GXX} -Wodr -Werror -I${SCRATCHDIR} -c ${SCRATCHDIR}/G${SRC}.cc -o ${SCRATCHDIR}/G${SRC}.o ; then
	echo "${NAME}: Passed"
    else
	echo "${NAME}: Failed (${GXX} -Wodr -Werror -I${SCRATCHDIR} -c ${SCRATCHDIR}/G${SRC}.cc)"
	exit 1
    fi
}


function localvar () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="local variable creation"
    compile_def localvar
    compile_mod localvar
    verify localvar
}


function globalvar () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="global variable creation"
    compile_def globalvar
    compile_mod globalvar
    verify globalvar
}


function localvarassign () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="local variable assignment"
    compile_def localvarassign
    compile_mod localvarassign
    verify localvarassign
}


function globalvarassign () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="global variable assignment"
    compile_def globalvarassign
    compile_mod globalvarassign
    verify globalvarassign
}


function localproctype () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="local proctype creation"
    compile_def localproctype
    compile_mod localproctype
    verify localproctype
}


function globalproctype () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="global proctype creation"
    compile_def globalproctype
    compile_mod globalproctype
    verify globalproctype
}


function simpleopaque () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="simple opaque type creation"
    compile_def simpleopaque
    compile_mod simpleopaque
    verify simpleopaque
}


function simplelist () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="simple list opaque type field manipulation"
    compile_def simplelist
    compile_mod simplelist
    verify simplelist
}


function simplelistiter () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="simple iteration using opaque type field"
    compile_def simplelistiter
    compile_mod simplelistiter
    verify simplelistiter
}


function opaqueparam () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="opaque parameter and field assignment"
    compile_def opaqueparam
    compile_mod opaqueparam
    verify opaqueparam
}


function opaqueuse () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="opaque use"
    compile_def opaqueparam
    compile_def opaqueuse
    compile_mod opaqueuse
    verify opaqueuse
}


function opaquestr () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="opaque str"
    compile_def opaquestr
    compile_def opaqueusestr
    compile_mod opaqueusestr
    verify opaqueusestr
}


function opaquenew () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="opaque new"
    compile_def opaquenew
    compile_mod opaquenew
    verify opaquenew
}


function opaquefield () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="opaque field"
    compile_def opaquestr
    compile_def opaquefield
    compile_mod opaquefield
    verify opaquefield
}


function opaquevariant () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="opaque variant"
    compile_def opaquevariant
    compile_mod opaquevariant
    verify opaquevariant
}


function opaquevarparam () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="opaque variant"
    compile_def opaquevarparam
    compile_mod opaquevarparam
    verify opaquevarparam
}


function straddress () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="string address"
    compile_def SYSTEM
    compile_def straddress
    compile_mod straddress
    verify straddress
}


function straddressexport () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="string address"
    compile_def SYSTEM
    compile_def straddressexport
    compile_mod straddressexport
    verify straddressexport
}


function unboundedarray () {
    rm -rf ${SCRATCHDIR}
    mkdir -p ${SCRATCHDIR}
    NAME="string address"
    compile_def unboundedarray
    compile_mod unboundedarray
    verify unboundedarray
}


function runall () {
    localvar
    globalvar
    localvarassign
    globalvarassign
    localproctype
    globalproctype
    simpleopaque
    simplelist
    simplelistiter
    opaqueparam
    opaqueuse
    opaquestr
    opaquenew
    opaquefield
    opaquevariant
    opaquevarparam
    straddress
    straddressexport
    unboundedarray
    echo "all base language boot strap tests pass -Wodr -Werror"
}


runall
