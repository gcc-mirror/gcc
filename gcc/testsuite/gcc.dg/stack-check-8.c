/* { dg-do run } */
/* { dg-options "-O2 -fstack-clash-protection -Wno-psabi -fno-optimize-sibling-calls --param stack-clash-protection-probe-interval=12 --param stack-clash-protection-guard-size=12" } */
/* { dg-require-effective-target supports_stack_clash_protection } */


typedef float V __attribute__((vector_size (32)));

__attribute__((noinline, noclone)) void
foo (char *p)
{
  asm volatile ("" : : "r" (p) : "memory");
}

__attribute__((noinline, noclone)) int
f0 (int x, int y)
{
  asm volatile ("" : : : "memory");  
  return x + y;
}

__attribute__((noinline, noclone)) void
f1 (void)
{
  char buf[64];
  foo (buf);
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  char buf[12000];
  foo (buf);
}

__attribute__((noinline, noclone)) void
f3 (void)
{
  char buf[131072];
  foo (buf);
}

__attribute__((noinline, noclone)) void
f4 (int x)
{
  char vla[x];
  foo (vla);
}

__attribute__((noinline, noclone)) void
f5 (int x)
{
  char buf[12000];
  foo (buf);
  {
    char vla[x];
    foo (vla);
  }
  {
    char vla[x];
    foo (vla);
  }
}

V v;

__attribute__((noinline, noclone)) int
f6 (int x, int y, V a, V b, V c)
{
  asm volatile ("" : : : "memory");  
  v = a + b + c;
  return x + y;
}

__attribute__((noinline, noclone)) void
f7 (V a, V b, V c)
{
  char buf[64];
  foo (buf);
  v = a + b + c;
}

__attribute__((noinline, noclone)) void
f8 (V a, V b, V c)
{
  char buf[12000];
  foo (buf);
  v = a + b + c;
}

__attribute__((noinline, noclone)) void
f9 (V a, V b, V c)
{
  char buf[131072];
  foo (buf);
  v = a + b + c;
}

__attribute__((noinline, noclone)) void
f10 (int x, V a, V b, V c)
{
  char vla[x];
  foo (vla);
  v = a + b + c;
}

__attribute__((noinline, noclone)) void
f11 (int x, V a, V b, V c)
{
  char buf[12000];
  foo (buf);
  v = a + b + c;
  {
    char vla[x];
    foo (vla);
  }
  {
    char vla[x];
    foo (vla);
  }
}

int
main ()
{
  f0 (2, 3);
  f1 ();
  f2 ();
  f3 ();
  f4 (12000);
  f5 (12000);
  f6 (2, 3, v, v, v);
  f7 (v, v, v);
  f8 (v, v, v);
  f9 (v, v, v);
  f10 (12000, v, v, v);
  f11 (12000, v, v, v);
  return 0;
}

