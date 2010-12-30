/* PR rtl-optimization/46865 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern unsigned long f;

#define m1(f)							\
  if (f & 1)							\
    asm volatile ("nop /* asmnop */\n");			\
  else								\
    asm volatile ("nop /* asmnop */\n");

#define m2(f)							\
  if (f & 1)							\
    asm volatile ("nop /* asmnop */\n" : : "i" (6) : "cx");	\
  else								\
    asm volatile ("nop /* asmnop */\n" : : "i" (6) : "cx");

void
foo (void)
{
  m1 (f);
}

void
bar (void)
{
  m2 (f);
}

/* { dg-final { scan-assembler-times "asmnop" 2 } } */
