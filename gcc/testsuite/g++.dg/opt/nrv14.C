// PR c++/32992
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort (void);

struct A
{
  long int a1;
  long int a2;
  long int a3;
};

struct B
{
  long int f[3];
  operator A ()
  {
    union
    {
      long int t[3];
      A a;
    };
    for (int i = 0; i < 3; i++)
      t[i] = f[i];
    return a;
  }
};

int
main ()
{
  B b = { {1, 3, 5} };
  A a = b;

  if (a.a1 != b.f[0] || a.a2 != b.f[1] || a.a3 != b.f[2])
    abort ();
  return 0;
}
