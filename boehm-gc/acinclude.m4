# Copyright (c) 1999-2001, 2002 by Red Hat, Inc. All rights reserved.
# 
# THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
# OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
# 
# Permission is hereby granted to use or copy this program
# for any purpose,  provided the above notices are retained on all copies.
# Permission to modify the code and to distribute modified code is granted,
# provided the above notices are retained, and a notice that the code was
# modified is included with the above copyright notice.
#
# Original author: Tom Tromey

AC_DEFUN(GC_CONFIGURE,
[

AM_INIT_AUTOMAKE(gc, 6.1a1, no-define)

# The autoconf 2.5x version of the no-executables hack.
sinclude(../config/no-executables.m4)
GCC_NO_EXECUTABLES

# Yak.  We must force CC and CXX to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS and CXXFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_CXX
m4_rename([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

AC_PROG_INSTALL

AM_MAINTAINER_MODE

. [$]{srcdir}/configure.host

case [$]{gc_basedir} in
/* | [A-Za-z]:[/\\]*) gc_flagbasedir=[$]{gc_basedir} ;;
*) gc_flagbasedir='[$](top_builddir)/'[$]{gc_basedir} ;;
esac

gc_cflags="[$]{gc_cflags} -I"'[$](top_builddir)'"/$1/targ-include -I[$]{gc_flagbasedir}/libc/include"
case "${host}" in
  *-*-cygwin32*)
    gc_cflags="[$]{gc_cflags} -I[$]{gc_flagbasedir}/../winsup/include"
    ;;
esac

dnl gc_cflags="[$]{gc_cflags} -fno-builtin"

GC_CFLAGS=${gc_cflags}
AC_SUBST(GC_CFLAGS)
])

sinclude(../libtool.m4)
dnl The line below arranges for aclocal not to bring a definition of
dnl AM_PROG_LIBTOOL into aclocal.m4, while still arranging for automake
dnl to add a definition of LIBTOOL to Makefile.in.
ifelse(yes,no,[AC_DEFUN([AM_PROG_LIBTOOL],[AC_SUBST(LIBTOOL)])])
