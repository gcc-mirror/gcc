/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler-times {\mendbr} 2 } } */

struct ucontext;

extern int (*bar) (struct ucontext *);

extern int res;

void
foo (struct ucontext *oucp)
{
  int (*f) (struct ucontext *) __attribute__((__indirect_return__))
    = bar;
  res = f (oucp);
}
