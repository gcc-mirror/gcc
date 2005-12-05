/* PR target/25199 */
/* { dg-do compile } */
/* { dg-options "-Os -mtune=pentium4" } */
/* { dg-require-effective-target ilp32 } */

struct S
{
  void *p[30];
  unsigned char c[4];
};

unsigned char d;

void
foo (struct S *x)
{
  register unsigned char e __asm ("esi");
  e = x->c[3];
  __asm __volatile ("" : : "r" (e));
  e = x->c[0];
  __asm __volatile ("" : : "r" (e));
}

/* { dg-final { scan-assembler-not "movl\[ \t\]*123" } } */
/* { dg-final { scan-assembler "movzbl\[ \t\]*123" } } */
/* { dg-final { scan-assembler "movl\[ \t\]*120" } } */
