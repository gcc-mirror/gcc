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

sinclude(../libtool.m4)
sinclude(../config/acx.m4)
dnl The line below arranges for aclocal not to bring a definition of
dnl AM_PROG_LIBTOOL into aclocal.m4, while still arranging for automake
dnl to add a definition of LIBTOOL to Makefile.in.
ifelse(yes,no,[AC_DEFUN([AM_PROG_LIBTOOL],[AC_SUBST(LIBTOOL)])])
