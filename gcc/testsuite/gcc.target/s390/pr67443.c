/* Test case for PR/67443.  */

/* { dg-do run { target s390*-*-* } } */
/* { dg-prune-output "call-clobbered register used for global register variable" } */
/* { dg-options "-march=z900 -fPIC -fomit-frame-pointer -O3 -save-temps" } */

#include <assert.h>

struct s_t
{
  unsigned f1 : 8;
  unsigned f2 : 24;
};

__attribute__ ((noinline))
int bar ()
{
  return 0;
}

__attribute__ ((noinline))
void foo (struct s_t *ps, int c)
{
  int tmp;

  /* Uses r2 as address register.  */
  ps->f1 = c;
  /* Clobber all registers that r2 could be stored into.  */
  __asm__ __volatile__ ("" : : : "memory",
			"r0","r1","r6","r7","r8","r9","r10","r11");
  /* Force that the pointer is evicted from r2 and stored on the stack.  */
  tmp = bar ();
  /* User the pointer again.  It gets reloaded to a different register because
     r2 is already occupied.  */
  ps->f2 = tmp;
  /* If dead store elimination fails to detect that the address in r2 during
     the first assignment is an alias of the address in rX during the second
     assignment, it eliminates the first assignment and the f1 field is not
     written (bug).  */
}
/* Make sure that r2 is used only once as an address register for storing.
   If this check fails, the test case needs to be fixed.
   { dg-final { scan-assembler-times "\tst.\?\t.*,0\\(%r2\\)" 1 } } */

int main (void)
{
  struct s_t s = { 0x01u, 0x020304u };

  foo (&s, 0);
  assert (s.f1 == 0&& s.f2 == 0);

  return 0;
}
