/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler "jmp" } } */

struct ucontext;

extern int (*bar) (struct ucontext *) __attribute__((__indirect_return__));
extern int foo (struct ucontext *) __attribute__((__indirect_return__));

int
foo (struct ucontext *oucp)
{
  return bar (oucp);
}
