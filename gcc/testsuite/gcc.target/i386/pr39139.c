/* PR target/39139 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

#ifdef __x86_64__
# define AX_REG asm ("rax")
# define DI_REG asm ("rdi")
# define SI_REG asm ("rsi")
#else
# define AX_REG asm ("eax")
# define DI_REG asm ("edi")
# define SI_REG asm ("esi")
#endif

static inline int
foo (unsigned int x, void *y)
{
  register unsigned long r AX_REG;
  register unsigned long a1 DI_REG;
  register unsigned long a2 SI_REG;
  a1 = (unsigned long) x;
  a2 = (unsigned long) y;
  asm volatile ("" : "=r" (r), "+r" (a1), "+r" (a2) : : "memory");
  return (int) r;
}

struct T { unsigned long t1, t2; unsigned int t3, t4, t5; };

int
bar (unsigned long x, unsigned int y, unsigned long u, unsigned int v)
{
  long r;
  struct T e = { .t1 = x, .t2 = u };

  if (x << y != u << v)
    return 5;
  r = foo (11, &e);
  return e.t3 == x;
}
