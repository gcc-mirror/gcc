/* Copyright (C) 2000 Free Software Foundation, Inc.  */
/* { dg-do compile } */

/* Since 1.0e and + form the pasted token, 1 is a separate token and
   so should be output with a preceding space.  The old preprocessor
   gets this wrong.  */

#define glue(x, y) x ## y

int main ()
{
  double d = glue (1.0e, +1); /* { dg-error "floating const|parse error" } */
  return 0;
}
