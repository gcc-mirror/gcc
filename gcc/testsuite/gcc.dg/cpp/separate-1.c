/* Copyright 2003 Free Software Foundation.  */

/* { dg-do compile } */
/* { dg-options "-save-temps" } */

/* Make sure we report errors in the right line, even if separate
   preprocessing is used.  */

#define FOO()

int FOO(
	), bar; /* { dg-error "(parse|syntax) error" "error on this line" } */
