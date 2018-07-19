/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

struct ucontext;

typedef int (*bar_p) (struct ucontext *)
  __attribute__((__indirect_return__));

extern int res;

void
foo (bar_p bar, struct ucontext *oucp)
{
  res = bar (oucp);
}
