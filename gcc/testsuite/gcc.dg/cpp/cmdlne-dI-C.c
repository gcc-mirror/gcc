/* Copyright (C) 2005 Free Software Foundation, Inc.  */
/* PR 13726 */

/* { dg-do preprocess } */
/* { dg-options "-dI -C" } */

#include "cmdlne-dI-C.h" /* #include comment */
/* comment 2 */

/* { dg-final { scan-file cmdlne-dI-C.i "#include c+omment" } } */
/* { dg-final { scan-file cmdlne-dI-C.i "header file c+omment" } } */
