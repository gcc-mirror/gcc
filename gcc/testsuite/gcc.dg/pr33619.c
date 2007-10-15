/* PR tree-optimization/33619 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#ifdef __powerpc__
# define REG1 __asm__ ("3")
# define REG2 __asm__ ("4")
#elif defined __x86_64__
# define REG1 __asm__ ("rdi")
# define REG2 __asm__ ("rsi")
#else
# define REG1
# define REG2
#endif

static inline void
bar (unsigned long x, int y)
{
  register unsigned long p1 REG1 = x;
  register unsigned long p2 REG2 = y;
  __asm__ volatile ("" : "=r" (p1), "=r" (p2) : "0" (p1), "1" (p2) : "memory");
  if (p1 != 0xdeadUL || p2 != 0xbefUL)
    __builtin_abort ();
}

__attribute__((const, noinline)) int
baz (int x)
{
  return x;
}

__attribute__((noinline)) void
foo (unsigned long *x, int y)
{
  unsigned long a = *x;
  bar (a, baz (y));
}

int
main (void)
{
  unsigned long a = 0xdeadUL;
  foo (&a, 0xbefUL);
  return 0;
}
