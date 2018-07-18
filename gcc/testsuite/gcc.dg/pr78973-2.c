/* PR c/78973 - warning: ‘memcpy’: specified size exceeds maximum object
   size [-Wstringop-overflow=]

   This is a companion test for the bug above that verifies that the correct
   range of the int variable is detected.

   { dg-do compile }
   { dg-require-effective-target int32plus }
   { dg-options "-O2 -Walloc-size-larger-than=4" }  */

void *p;

void f (int n)
{
  if (n <= 4)
    p = __builtin_malloc (n);
  /* { dg-warning "argument 1 range \\\[\[0-9\]+, \[0-9\]+\\\] exceeds maximum object size 4" "ilp32" { xfail { ! lp64 } } .-1 } */
}

void g (unsigned n)
{
  if (n < 5)
    n = 5;
  f (n);
}
