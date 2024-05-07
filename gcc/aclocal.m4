# generated automatically by aclocal 1.15.1 -*- Autoconf -*-

# Copyright (C) 1996-2017 Free Software Foundation, Inc.

# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

m4_ifndef([AC_CONFIG_MACRO_DIRS], [m4_defun([_AM_CONFIG_MACRO_DIRS], [])m4_defun([AC_CONFIG_MACRO_DIRS], [_AM_CONFIG_MACRO_DIRS($@)])])
# AM_CONDITIONAL                                            -*- Autoconf -*-

# Copyright (C) 1997-2017 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# AM_CONDITIONAL(NAME, SHELL-CONDITION)
# -------------------------------------
# Define a conditional.
AC_DEFUN([AM_CONDITIONAL],
[AC_PREREQ([2.52])dnl
 m4_if([$1], [TRUE],  [AC_FATAL([$0: invalid condition: $1])],
       [$1], [FALSE], [AC_FATAL([$0: invalid condition: $1])])dnl
AC_SUBST([$1_TRUE])dnl
AC_SUBST([$1_FALSE])dnl
_AM_SUBST_NOTMAKE([$1_TRUE])dnl
_AM_SUBST_NOTMAKE([$1_FALSE])dnl
m4_define([_AM_COND_VALUE_$1], [$2])dnl
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi
AC_CONFIG_COMMANDS_PRE(
[if test -z "${$1_TRUE}" && test -z "${$1_FALSE}"; then
  AC_MSG_ERROR([[conditional "$1" was never defined.
Usually this means the macro was only invoked conditionally.]])
fi])])

# Copyright (C) 2006-2017 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# _AM_SUBST_NOTMAKE(VARIABLE)
# ---------------------------
# Prevent Automake from outputting VARIABLE = @VARIABLE@ in Makefile.in.
# This macro is traced by Automake.
AC_DEFUN([_AM_SUBST_NOTMAKE])

# AM_SUBST_NOTMAKE(VARIABLE)
# --------------------------
# Public sister of _AM_SUBST_NOTMAKE.
AC_DEFUN([AM_SUBST_NOTMAKE], [_AM_SUBST_NOTMAKE($@)])

m4_include([../libtool.m4])
m4_include([../ltoptions.m4])
m4_include([../ltsugar.m4])
m4_include([../ltversion.m4])
m4_include([../lt~obsolete.m4])
m4_include([../config/acx.m4])
m4_include([../config/cet.m4])
m4_include([../config/codeset.m4])
m4_include([../config/depstand.m4])
m4_include([../config/dfp.m4])
m4_include([../config/enable.m4])
m4_include([../config/gcc-plugin.m4])
m4_include([../config/gettext-sister.m4])
m4_include([../config/gettext.m4])
m4_include([../config/iconv.m4])
m4_include([../config/intlmacosx.m4])
m4_include([../config/lcmessage.m4])
m4_include([../config/lead-dot.m4])
m4_include([../config/lib-ld.m4])
m4_include([../config/lib-link.m4])
m4_include([../config/lib-prefix.m4])
m4_include([../config/mmap.m4])
m4_include([../config/nls.m4])
m4_include([../config/override.m4])
m4_include([../config/picflag.m4])
m4_include([../config/po.m4])
m4_include([../config/progtest.m4])
m4_include([../config/stdint.m4])
m4_include([../config/warnings.m4])
m4_include([../config/zlib.m4])
m4_include([acinclude.m4])
