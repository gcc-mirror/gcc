// PR c++/47476
// { dg-do compile }
// { dg-options "-std=c++0x" }

int
foo (int a, int b)
{
  const bool c ((a != 0) == (b != 26));
  return c;
}
