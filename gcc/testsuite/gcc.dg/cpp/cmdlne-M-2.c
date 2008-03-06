/* Copyright (C) 2008 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-M" } */

/* Test that dependency output suppresses warnings by implying -w.  */

#include "#cmdlne-M-2.h"
#include "cmdlne-M-2#.h"

/* { dg-final { scan-file cmdlne-M-2.i "(^|\\n)cmdlne-M-2.o:" } }
   { dg-final { scan-file cmdlne-M-2.i "cmdlne-M-2.c" } }
   { dg-final { scan-file cmdlne-M-2.i "\\\\#cmdlne-M-2.h" } }
   { dg-final { scan-file cmdlne-M-2.i "cmdlne-M-2\\\\#.h" } } */
