/* Copyright (C) 2000 Free Software Foundation, Inc.  */
/* { dg-do compile } */
/* { dg-options -save-temps } */

/* Since 1.0e and + form the pasted token, 1 is a separate token and
   so should be output with a preceding space.  The old preprocessor
   gets this wrong.  We use -save-temps to avoid direct use of the
   integrated preprocessor.  */

#define glue(x, y) x ## y

int main ()
{
  double d = glue (1.0e, +1); /* { dg-error "exponent|parse error|syntax error|expected" } */
  return 0;
}
