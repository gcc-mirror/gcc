/* PR tree-optimization/9814  */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort(void);

int test1(int x)
{
  if (x & 2)
    x |= 2;
  return x;
}

int test2(int x)
{
  if (!(x & 2))
    x |= 2;
  return x;
}

int test3(int x)
{
  if (x & 2)
    x ^= 2;
  return x;
}

int test4(int x)
{
  if (!(x & 2))
    x ^= 2;
  return x;
}

int test5(int x)
{
  if (x & 2)
    x &= ~2;
  return x;
}

int test6(int x)
{
  if (!(x & 2))
    x &= ~2;
  return x;
}

int main()
{
  if (test1(0) != 0)
    abort();
  if (test1(2) != 2)
    abort();
  if (test1(5) != 5)
    abort();
  if (test1(7) != 7)
    abort();

  if (test2(0) != 2)
    abort();
  if (test2(2) != 2)
    abort();
  if (test2(5) != 7)
    abort();
  if (test2(7) != 7)
    abort();

  if (test3(0) != 0)
    abort();
  if (test3(2) != 0)
    abort();
  if (test3(5) != 5)
    abort();
  if (test3(7) != 5)
    abort();

  if (test4(0) != 2)
    abort();
  if (test4(2) != 2)
    abort();
  if (test4(5) != 7)
    abort();
  if (test4(7) != 7)
    abort();

  if (test5(0) != 0)
    abort();
  if (test5(2) != 0)
    abort();
  if (test5(5) != 5)
    abort();
  if (test5(7) != 5)
    abort();

  if (test6(0) != 0)
    abort();
  if (test6(2) != 2)
    abort();
  if (test6(5) != 5)
    abort();
  if (test6(7) != 7)
    abort();

  return 0;
}

