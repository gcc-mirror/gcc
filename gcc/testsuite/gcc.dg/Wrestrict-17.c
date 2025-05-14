/* PR tree-optimization/85826 - ICE in gimple-ssa-warn-restruct on
   a variable-length struct
   { dg-do compile }
   { dg-options "-O2 -Wall" }  */

int f (int n)
{
  typedef struct { int a[n]; } S;

  S a;
  __attribute__ ((noinline)) S g (void) { return a; }

  a.a[0] = 1;
  a.a[9] = 2;

  S b;
  b = g ();

  return b.a[0] == 1 && b.a[9] == 2;
}
