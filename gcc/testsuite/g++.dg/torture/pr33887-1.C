/* { dg-do run } */

extern "C" void abort (void);
struct S { unsigned int i : 24; } x;
void __attribute__((noinline)) test1()
{
  if (--x.i != 0x00ffffff)
    abort ();
  if (x.i != 0x00ffffff)
    abort ();
}
void __attribute__((noinline)) test2()
{
  if (x.i-- != 0)
    abort ();
  if (x.i != 0x00ffffff)
    abort ();
}
void __attribute__((noinline)) test3()
{
  if (++x.i != 0)
    abort ();
  if (x.i != 0)
    abort ();
}
void __attribute__((noinline)) test4()
{
  if (x.i++ != 0x00ffffff)
    abort ();
  if (x.i != 0)
    abort ();
}
int main()
{
  x.i = 0;
  test1();
  x.i = 0;
  test2();
  x.i = 0x00ffffff;
  test3();
  x.i = 0x00ffffff;
  test4();
  return 0;
}
