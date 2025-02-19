// PR c++/86769
// { dg-do run }
// { dg-options "-O1" }

__attribute__((noipa)) void
foo (int)
{
  static int a = 0;
  if (++a == 3)
    __builtin_abort ();
}

int
main ()
{
  volatile int x = 10;
  for (int l = 1; int d = x - l; l = d + 1)
    {
      int &z = d;
      foo (z);
    }
}
