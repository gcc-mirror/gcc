/* Copyright (C) 2001, 2003 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* We used to output an unnecessary leading space, leading to Emacs
   confusion with its Makefile abuse.

   Neil Booth, 12 Oct 2001.  */

#define EMPTY
#define foo bar

a = EMPTY
foo..				/* No leading space on output.  */

/* { dg-final { scan-file spacing2.i "(^|\n)bar\.\." } } */
