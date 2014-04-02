// PR c++/60227
// { dg-do compile { target c++1y } }

void foo(int n)
{
  int a[n];
  int (&r)[n] = {};		// { dg-error "" }
}
