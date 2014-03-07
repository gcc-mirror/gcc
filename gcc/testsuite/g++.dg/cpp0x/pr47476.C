// PR c++/47476
// { dg-do compile { target c++11 } }

int
foo (int a, int b)
{
  const bool c ((a != 0) == (b != 26));
  return c;
}
