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

# FIXME: We temporarily define our own version of AC_PROG_CC.  This is
# copied from autoconf 2.12, but does not call AC_PROG_CC_WORKS.  We
# are probably using a cross compiler, which will not be able to fully
# link an executable.  This should really be fixed in autoconf
# itself.

AC_DEFUN(GC_CONFIGURE,
[
dnl Default to --enable-multilib
AC_ARG_ENABLE(multilib,
[  --enable-multilib       build many library versions (default)],
[case "${enableval}" in
  yes) multilib=yes ;;
  no)  multilib=no ;;
  *)   AC_MSG_ERROR(bad value ${enableval} for multilib option) ;;
 esac], [multilib=yes])dnl

dnl We may get other options which we don't document:
dnl --with-target-subdir, --with-multisrctop, --with-multisubdir

dnl I needed to add the -n test to allow configuration in src directory - HB
if test "[$]{srcdir}" = "."; then
  if test "[$]{with_target_subdir}" != "." -a -n "[$]{with_target_subdir}"; then
    gc_basedir="[$]{srcdir}/[$]{with_multisrctop}../$1"
  else
    gc_basedir="[$]{srcdir}/[$]{with_multisrctop}$1"
  fi
else
  gc_basedir="[$]{srcdir}/$1"
fi
AC_SUBST(gc_basedir)
AC_CONFIG_AUX_DIR($gc_basedir/..)
if :; then :; else
  # This overrides the previous occurrence for automake, but not for
  # autoconf, which is exactly what we want.
  AC_CONFIG_AUX_DIR(..)
fi

AC_CANONICAL_SYSTEM

# This works around an automake problem.
mkinstalldirs="`cd $ac_aux_dir && ${PWDCMD-pwd}`/mkinstalldirs"
AC_SUBST(mkinstalldirs)

AM_INIT_AUTOMAKE(gc, 6.1a1, no-define)

# FIXME: We temporarily define our own version of AC_PROG_CC.  This is
# copied from autoconf 2.12, but does not call AC_PROG_CC_WORKS.  We
# are probably using a cross compiler, which will not be able to fully
# link an executable.  This should really be fixed in autoconf
# itself.

AC_DEFUN(LIB_AC_PROG_CC,
[AC_BEFORE([$0], [AC_PROG_CPP])dnl
dnl Fool anybody using AC_PROG_CC.
AC_PROVIDE([AC_PROG_CC])
AC_CHECK_PROG(CC, gcc, gcc)
if test -z "$CC"; then
  AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc)
  test -z "$CC" && AC_MSG_ERROR([no acceptable cc found in \$PATH])
fi

AC_PROG_CC_GNU

if test $ac_cv_prog_gcc = yes; then
  GCC=yes
dnl Check whether -g works, even if CFLAGS is set, in case the package
dnl plays around with CFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CFLAGS="${CFLAGS+set}"
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=
  AC_PROG_CC_G
  if test "$ac_test_CFLAGS" = set; then
    CFLAGS="$ac_save_CFLAGS"
  elif test $ac_cv_prog_cc_g = yes; then
    CFLAGS="-g -O2"
  else
    CFLAGS="-O2"
  fi
else
  GCC=
  test "${CFLAGS+set}" = set || CFLAGS="-g"
fi
])

LIB_AC_PROG_CC

# Likewise for AC_PROG_CXX.
AC_DEFUN(LIB_AC_PROG_CXX,
[AC_BEFORE([$0], [AC_PROG_CXXCPP])dnl
dnl Fool anybody using AC_PROG_CXX.
AC_PROVIDE([AC_PROG_CXX])
AC_CHECK_PROGS(CXX, $CCC c++ g++ gcc CC cxx cc++, gcc)
test -z "$CXX" && AC_MSG_ERROR([no acceptable c++ found in \$PATH])

AC_PROG_CXX_GNU

if test $ac_cv_prog_gxx = yes; then
  GXX=yes
dnl Check whether -g works, even if CXXFLAGS is set, in case the package
dnl plays around with CXXFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS=
  AC_PROG_CXX_G
  if test "$ac_test_CXXFLAGS" = set; then
    CXXFLAGS="$ac_save_CXXFLAGS"
  elif test $ac_cv_prog_cxx_g = yes; then
    CXXFLAGS="-g -O2"
  else
    CXXFLAGS="-O2"
  fi
else
  GXX=
  test "${CXXFLAGS+set}" = set || CXXFLAGS="-g"
fi
])

LIB_AC_PROG_CXX

# AC_CHECK_TOOL does AC_REQUIRE (AC_CANONICAL_BUILD).  If we don't
# run it explicitly here, it will be run implicitly before
# NEWLIB_CONFIGURE, which doesn't work because that means that it will
# be run before AC_CANONICAL_HOST.
AC_CANONICAL_BUILD

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
