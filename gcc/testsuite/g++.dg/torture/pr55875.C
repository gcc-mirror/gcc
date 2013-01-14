// { dg-do run }

struct A
{
  short int a1;
  unsigned char a2;
  unsigned int a3;
};

struct B
{
  unsigned short b1;
  const A *b2;
};

B b;

__attribute__((noinline, noclone))
int foo (unsigned x)
{
  __asm volatile ("" : "+r" (x) : : "memory");
  return x;
}

inline void
bar (const int &)
{
}

__attribute__((noinline)) void
baz ()
{
  const A *a = b.b2;
  unsigned int i;
  unsigned short n = b.b1;
  for (i = 0; i < n; ++i)
    if (a[i].a1 == 11)
      {
    if (i > 0 && (a[i - 1].a2 & 1))
      continue;
    bar (foo (2));
    return;
      }
}

int
main ()
{
  A a[4] = { { 10, 0, 0 }, { 11, 1, 0 }, { 11, 1, 0 }, { 11, 1, 0 } };
  b.b1 = 4;
  b.b2 = a;
  baz ();
  return 0;
}

