/* Copyright (C) 2000, 2003 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-P" } */

/* Test that we don't stair-step output with -P.  Source: Neil Booth,
   18 Dec 2000.  */

int x = 1;

/* { dg-final { scan-file cmdlne-P.i "(^|\n)int x = 1;($|\n)" } } */
