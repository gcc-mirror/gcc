/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* We used to output an unnecessary leading space, leading to Emacs
   confusion with its Makefile abuse.

   Neil Booth, 12 Oct 2001.  */

#define EMPTY
#define foo bar

a = EMPTY
foo..				/* No leading space on output.  */

/*
   { dg-final { if ![file exists spacing2.i] { return }                   } }
   { dg-final { if \{ [grep spacing2.i "^bar\.\."] != "" \}           \{  } }
   { dg-final { return \}                                                 } }
   { dg-final { fail "spacing2.c: spacing issues"			  } }
*/
