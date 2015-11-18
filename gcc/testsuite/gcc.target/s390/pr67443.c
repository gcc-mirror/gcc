/* Test case for PR/67443.  */

/* { dg-do run { target s390*-*-* } } */
/* { dg-prune-output "call-clobbered register used for global register variable" } */
/* { dg-options "-march=z900 -fPIC -fomit-frame-pointer -O3" } */

#include <assert.h>

/* Block all registers except the first three argument registers.  */
register long r0 asm ("r0");
register long r1 asm ("r1");
register long r5 asm ("r5");
register long r6 asm ("r6");
register long r7 asm ("r7");
register long r8 asm ("r8");
register long r9 asm ("r9");
register long r10 asm ("r10");
register long r11 asm ("r11");

struct s_t
{
  unsigned f1 : 8;
  unsigned f2 : 24;
};

__attribute__ ((noinline))
void foo (struct s_t *ps, int c, int i)
{
  /* Uses r2 as address register.  */
  ps->f1 = c;
  /* The calculation of the value is so expensive that it's cheaper to spill ps
     to the stack and reload it later (into a different register).
     ==> Uses r4 as address register.*/
  ps->f2 = i + i % 3;
  /* If dead store elimination fails to detect that the address in r2 during
     the first assignment is an alias of the address in r4 during the second
     assignment, it eliminates the first assignment and the f1 field is not
     written (bug).  */
}

int main (void)
{
  struct s_t s = { 0x01u, 0x020304u };

  foo (&s, 0, 0);
  assert (s.f1 == 0&& s.f2 == 0);

  return 0;
}
