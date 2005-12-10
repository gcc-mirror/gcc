/* Check that indirect sibcalls understand regparm.  */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2" } */

extern void abort (void);

int (*f)(int, int) __attribute__((regparm(2)));
int (*g)(int, int, int) __attribute__((regparm(3)));

int __attribute__((noinline))
foo(void)
{
  return f(1, 2);
}

int __attribute__((noinline))
bar(void)
{
  return g(1, 2, 3);
}

int __attribute__((regparm(2)))
f1(int x, int y)
{
  return x*3 + y;
}

int __attribute__((regparm(3)))
g1(int x, int y, int z)
{
  return x*9 + y*3 + z;
}

int main()
{
  f = f1;
  g = g1;
  if (foo() != 1*3 + 2)
    abort ();
  if (bar() != 1*9 + 2*3 + 3)
    abort ();
  return 0;
}
