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

__extension__ typedef __SIZE_TYPE__ size_t;

static inline int
foo (unsigned int x, void *y)
{
  register size_t r AX_REG;
  register size_t a1 DI_REG;
  register size_t a2 SI_REG;
  a1 = (size_t) x;
  a2 = (size_t) y;
  asm volatile ("" : "=r" (r), "+r" (a1), "+r" (a2) : : "memory");
  return (int) r;
}

struct T { size_t t1, t2; unsigned int t3, t4, t5; };

int
bar (size_t x, unsigned int y, size_t u, unsigned int v)
{
  long r;
  struct T e = { .t1 = x, .t2 = u };

  if (x << y != u << v)
    return 5;
  r = foo (11, &e);
  return e.t3 == x;
}
