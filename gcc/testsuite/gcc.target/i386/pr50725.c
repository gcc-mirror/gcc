/* PR target/50725 */
/* { dg-do run { target avx_runtime } } */
/* { dg-options "-O2 -mavx" } */

extern void abort (void);

typedef int __attribute__((vector_size (32))) m256i;

__attribute__((noinline, noclone)) void
foo (int *x, m256i *y)
{
  asm volatile ("" : : "r" (x), "r" (y) : "memory");
}

__attribute__((noinline, noclone)) int
bar (int x)
{
  if (x > 20)
    return 24;
  m256i i;
  foo (__builtin_alloca (x), &i);
  return 128;
}

__attribute__((noinline, noclone)) int
baz (int d0, int d1, int d2, int d3, int d4, int d5, int x)
{
  if (x > 20)
    return 24;
  m256i i;
  d0 += d1 + d2 + d3 + d4 + d5; d1 += d0;
  foo (__builtin_alloca (x), &i);
  return 128;
}

int
main ()
{
  if (bar (22) != 24 || bar (20) != 128)
    abort ();
#ifdef __x86_64__
  register long r10 __asm__ ("r10") = 0xdeadbeefdeadbeefUL;
  asm volatile ("" : "+r" (r10));
#endif
  if (baz (0, 0, 0, 0, 0, 0, 22) != 24 || baz (0, 0, 0, 0, 0, 0, 20) != 128)
    abort ();
  return 0;
}
