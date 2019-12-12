/* PR tree-optimization/84053] missing -Warray-bounds accessing
   a local array across inlined function boundaries
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

static int deref (const int *p, int i)
{
  return p[i];                // { dg-warning "array subscript \\\[3, \[0-9\]+] is outside array bounds of .int\\\[2\\\]." "ilp32" { xfail ilp32 } }

  // There should also be an inlining context here.  PR 86650 tracks
  // its absence.
}

static int deref_3_plus (const int *p, int i)
{
  if (i < 3)
    i = 3;

  return deref (p, i);
}

int deref_a (int i)
{
  int a[] = { 2, 3 };

  return deref_3_plus (a, i);
}
