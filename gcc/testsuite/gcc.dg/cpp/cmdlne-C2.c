/* Copyright (C) 2000, 2001, 2003 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-C" } */

/* This tests that C++ comments are either dropped, or converted to C
   comments in macro expansions.  The + in the regexp stops it from
   matching itself 8-)

   Neil Booth, 9 Oct 2001.  */

#define ZERO 0 // A comment

ZERO:

/* { dg-final { scan-file-not cmdlne-C2.i "c+omment:" } } */

