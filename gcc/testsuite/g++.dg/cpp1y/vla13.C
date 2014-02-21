// PR c++/60277
// { dg-options "-std=c++1y -pedantic-errors" }

void foo(int n)
{
  int a[n];
  int (&r)[n] = {};		// { dg-error "" }
}
