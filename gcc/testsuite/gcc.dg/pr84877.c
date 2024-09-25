/* { dg-do run { xfail { { cris-*-* sparc*-*-* } || { { ! lp64 } && hppa*-*-* } } } } */
/* { dg-options "-O2" } */

#include <inttypes.h>

#ifdef __CRIS__
#define OUTGOING_SP_OFFSET (-sizeof (void *))
/* Suggestion: append #elif defined(__<TARGET-IDENTIFIER>__) after this comment,
   either defining OUTGOING_SP_OFFSET to whatever the pertinent amount is at -O2,
   if that makes your target consistently fail this test, or define
   DO_NOT_TAMPER for more complicated situations.  Either way, compile with
   -DDO_NO_TAMPER to avoid any meddling.  */
#endif

#if defined (OUTGOING_SP_OFFSET) && !defined (DO_NOT_TAMPER)
extern int xmain () __attribute__ ((__noipa__));
int main ()
{
  uintptr_t misalignment
    = (OUTGOING_SP_OFFSET
        + (15 & (uintptr_t) __builtin_stack_address ()));
  /* Allocate a minimal amount if the stack was accidentally aligned.  */
  void *q = __builtin_alloca (misalignment == 0);
  xmain ();
  /* Fake use to avoid the "allocation" being optimized out.  */
  asm volatile ("" : : "rm" (q));
  return 0;
}
#define main xmain
#endif

struct U {
    int M0;
    int M1;
} __attribute ((aligned (16)));

volatile struct U p0 = {1, 0};

void __attribute__ ((noinline))
foo (struct U p)
{

  volatile intptr_t mask = 0b1111;
  volatile int dummy[2];
  struct U p1 = p;
  dummy[1] = p.M0;

  if ((intptr_t)(&p1) & mask)
    __builtin_abort ();
  if ((intptr_t)(&p) & mask)
    __builtin_abort ();

  if (p1.M0 != dummy[1])
    __builtin_abort ();
  if (p1.M1 != p.M1)
    __builtin_abort ();
}

int
main ()
{
  foo (p0);
  return 0;
}
