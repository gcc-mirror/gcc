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

AC_PROG_CC
AC_PROG_CXX

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

AC_PROG_INSTALL

AM_MAINTAINER_MODE

# We need AC_EXEEXT to keep automake happy in cygnus mode.  However,
# at least currently, we never actually build a program, so we never
# need to use $(EXEEXT).  Moreover, the test for EXEEXT normally
# fails, because we are probably configuring with a cross compiler
# which can't create executables.  So we include AC_EXEEXT to keep
# automake happy, but we don't execute it, since we don't care about
# the result.
if false; then
  # autoconf 2.50 runs AC_EXEEXT by default, and the macro expands
  # to nothing, so nothing would remain between `then' and `fi' if it
  # were not for the `:' below.
  :
  AC_EXEEXT
fi

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
]))

))))

sinclude(../libtool.m4)
dnl The line below arranges for aclocal not to bring a definition of
dnl AM_PROG_LIBTOOL into aclocal.m4, while still arranging for automake
dnl to add a definition of LIBTOOL to Makefile.in.
ifelse(yes,no,[AC_DEFUN([AM_PROG_LIBTOOL],[AC_SUBST(LIBTOOL)])])
