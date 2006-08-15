/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do compile } */
/* { dg-options "-std=c89" } */

/* Just simple check that digraphs are not on in c89, for both
   preprocessor and compiler.  digraphs.c is the general test.  */

int main (int argc, char *argv[])
{
  return 0;
%>				/* { dg-error "parse error|syntax error|expected" } */

/* Place this after main () so we get to test both the compiler above
   and the preprocessor below.  */
%:define glue                   /* { dg-error "expected declaration" } */
#ifdef glue
#error glue is defined!
#endif
