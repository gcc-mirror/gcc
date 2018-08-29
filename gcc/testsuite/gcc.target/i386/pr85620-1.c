/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

struct ucontext;

extern int bar (struct ucontext *) __attribute__((__indirect_return__));

extern int res;

void
foo (struct ucontext *oucp)
{
  res = bar (oucp);
}
