/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test correct pasting of identifiers and numbers.  We can paste any
   number, as long as no '.', '-' or '+' appears in its spelling.  */

#define glue(x, y) x ## y

glue (ident, 12)		/* OK.  */
glue (ident, 12e3)		/* OK.  */
glue (ident, 12e+3)		/* { dg-warning "valid preprocessing tok" } */
glue (ident, 12e-3)		/* { dg-warning "valid preprocessing tok" } */
glue (ident, 1.2)		/* { dg-warning "valid preprocessing tok" } */
glue (ident, .12)		/* { dg-warning "valid preprocessing tok" } */
