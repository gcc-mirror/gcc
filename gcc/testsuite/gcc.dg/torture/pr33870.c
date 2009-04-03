/* { dg-do run } */

struct X {
  int i;
  int a[4];
} m;

int a[4];

int __attribute__((noinline)) foo(int b)
{
  int (*p)[4] = b ? &a : &m.a;
  a[3] = 0;
  (*p)[3] = 1;
  return (*p)[3] + (*p)[2] + (*p)[1] + a[0] + a[3];
}

extern void abort (void);

int main()
{
  int i;
  for (i = 0; i < 4; ++i)
    a[i] = 0;
  if (foo(1) != 2)
    abort ();
  return 0;
}

