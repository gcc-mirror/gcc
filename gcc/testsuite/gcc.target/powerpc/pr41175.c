/* PR target/41175 */
/* { dg-do run } */
/* { dg-options "-Os" } */

#define X2(n) X1(n##0) X1(n##1)
#define X4(n) X2(n##0) X2(n##1)
#define X8(n) X4(n##0) X4(n##1)

#ifndef __SPE__
#define FLOAT_REG_CONSTRAINT "f"
#else
#define FLOAT_REG_CONSTRAINT "r"
#endif

volatile int ll;

__attribute__((noinline)) void
foo (void)
{
  asm volatile ("" : : : "memory");
}

__attribute__((noinline)) void
bar (char *p)
{
  asm volatile ("" : : "r" (p) : "memory");
}

__attribute__((noinline)) void
f1 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
  foo ();
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f2 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
  char *pp = __builtin_alloca (ll);
  bar (pp);
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f3 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
}

#ifndef __NO_FPRS__
__attribute__((noinline)) void
f4 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X4(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X4(d) "=m" (mem) : : "memory");
  foo ();
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X4(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f5 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X4(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X4(d) "=m" (mem) : : "memory");
  char *pp = __builtin_alloca (ll);
  bar (pp);
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X4(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f6 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X4(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X4(d) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X4(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f7 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X2(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X2(d) "=m" (mem) : : "memory");
  foo ();
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X2(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f8 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X2(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X2(d) "=m" (mem) : : "memory");
  char *pp = __builtin_alloca (ll);
  bar (pp);
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X2(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f9 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X8(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X2(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X8(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X2(d) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X8(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X2(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f10 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X4(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X1(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X4(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X1(d) "=m" (mem) : : "memory");
  foo ();
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X4(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X1(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f11 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X4(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X1(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X4(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X1(d) "=m" (mem) : : "memory");
  char *pp = __builtin_alloca (ll);
  bar (pp);
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X4(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X1(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f12 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X4(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X1(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X4(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X1(d) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X4(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X1(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f13 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X2(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X8(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X2(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X8(d) "=m" (mem) : : "memory");
  foo ();
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X2(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X8(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f14 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X2(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X8(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X2(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X8(d) "=m" (mem) : : "memory");
  char *pp = __builtin_alloca (ll);
  bar (pp);
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X2(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X8(d) "m" (mem) : "memory");
}

__attribute__((noinline)) void
f15 (void)
{
  int mem;
#undef X1
#define X1(n) int gpr##n = 0;
  X8(a) X8(b) X2(c)
#undef X1
#define X1(n) double fpr##n = 0.0;
  X8(d)
#undef X1
#define X1(n) "+r" (gpr##n),
  asm volatile ("" : X8(a) "=m" (mem) : : "memory");
  asm volatile ("" : X8(b) "=m" (mem) : : "memory");
  asm volatile ("" : X2(c) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "+" FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : X8(d) "=m" (mem) : : "memory");
#undef X1
#define X1(n) "r" (gpr##n),
  asm volatile ("" : : X8(a) "m" (mem) : "memory");
  asm volatile ("" : : X8(b) "m" (mem) : "memory");
  asm volatile ("" : : X2(c) "m" (mem) : "memory");
#undef X1
#define X1(n) FLOAT_REG_CONSTRAINT (fpr##n),
  asm volatile ("" : : X8(d) "m" (mem) : "memory");
}
#endif

int
main ()
{
  ll = 60;
  f1 ();
  f2 ();
  f3 ();
#ifndef __NO_FPRS__
  f4 ();
  f5 ();
  f6 ();
  f7 ();
  f8 ();
  f9 ();
  f10 ();
  f11 ();
  f12 ();
  f13 ();
  f14 ();
  f15 ();
#endif
  return 0;
}
