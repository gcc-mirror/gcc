/* This testcase used to fail because outlining_inline_function was called
   too early, before rtl was generated.  */
/* { dg-do compile } */

int foo (const int *x)
{
  char a[*x];
  return 0;
}
