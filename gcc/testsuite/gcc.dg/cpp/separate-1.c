/* Copyright 2003 Free Software Foundation.  */

/* { dg-do compile } */
/* { dg-options "-save-temps" } */

/* Make sure we report errors in the right line, even if separate
   preprocessing is used.  */

#define FOO()

int FOO( /* { dg-error "parse error|syntax error|expected" "error on this line" } */
	), bar;

int baz FOO /* { dg-error "parse error|syntax error|expected" "error on this line" } */
; /* { dg-warning "no type or storage class" "warning on this line" } */
