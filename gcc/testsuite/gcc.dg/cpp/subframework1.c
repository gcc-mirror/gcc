/* Copyright (C) 2004 Free Software Foundation, Inc.  */

/* { dg-do preprocess { target *-*-darwin* } } */
/* { dg-options "-F$srcdir/gcc.dg/cpp/frame" } */

/* Contributed by Robert Bowdidge <bowdidge@apple.com>  */
/* include a file from a subframework that will import two files that
   both look at the same file.  Make sure we only include that file once;
   if so, the program will compile fine.  If not, we'll get redefinition
   errors */

#include <one/one-includeSubs.h>
#ifndef ONESUB_C_INCLUDED
#error C.h not included
#endif
