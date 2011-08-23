// PR c++/50158
// { dg-do compile }
// { dg-options "-Wunused" }

int bar (int);

int
foo (int a)
{
  int b[] = { a, -a };
  a += b[bar (a) < a];
  return a;
}
