/* Copyright (C) 2000, 2003 Free Software Foundation, Inc.  */

/* { dg-do compile } */

/* Test that multi-line tokens are rejected by the compiler.  Source:
   Neil Booth.  */

const char *p = "line 1
"
"";			      /* The compiler front end sees this.  */
/* { dg-error "17:missing term" "multiline strings" { target *-*-* } .-3 } */
/* { dg-error "missing term" "multiline strings" { target *-*-* } .-3 } */

