/* This testcase used to fail because outlining_inline_function was called
   too early, before rtl was generated.  */
/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

int foo (const int *x)
{
  char a[*x];
  return 0;
}
