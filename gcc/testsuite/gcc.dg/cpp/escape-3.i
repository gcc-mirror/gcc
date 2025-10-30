# 0 "escape-3.c"
# 0 "/some\\directory//"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "escape-3.c"

/* { dg-do compile } */
/* { dg-options "-g1" } */

int foo (int a, int b)
{
   return a + b;
}

/* Test for "/some\\directory" instead of "/some\\\\directory" */
/* { dg-final { scan-assembler "/some\\\\\\\\directory" } } */
