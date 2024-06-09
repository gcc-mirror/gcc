// PR c++/114691
// { dg-do compile }
// { dg-options "-O2 -Wall" }

void qux (int);
int foo (int);

void
bar (int x)
{
  #pragma GCC novector
  while (int y = foo (x))	// { dg-bogus "ignoring loop annotation" }
    qux (y);
}

void
baz (int x)
{
  #pragma GCC novector
  for (; int y = foo (x); )	// { dg-bogus "ignoring loop annotation" }
    qux (y);
}
