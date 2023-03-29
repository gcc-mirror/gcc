/* PR tree-optimization/86650 - -Warray-bounds missing inlining context
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds" } */

int a[3];           /* { dg-message "while referencing .a." } */
int x;

inline void foo (int i)
{
  a[i + 1] = 123;   /* { dg-warning "\\\[-Warray-bounds" } */
}

int bar (void)
{
  foo (3);

  return x;
}
