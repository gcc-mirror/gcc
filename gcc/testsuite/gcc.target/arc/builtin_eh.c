/* Check if we have the right offset for @bar function.  */
/* { dg-options "-O1" } */

void bar (void);

void
foo (int x)
{
  __builtin_unwind_init ();
  __builtin_eh_return (x, bar);
}

/* { dg-final { scan-assembler "r24" } } */
/* { dg-final { scan-assembler "r22" } } */
/* { dg-final { scan-assembler "r20" } } */
/* { dg-final { scan-assembler "r18" } } */
/* { dg-final { scan-assembler "r16" } } */
/* { dg-final { scan-assembler "r14" } } */
/* { dg-final { scan-assembler "r13" } } */
/* { dg-final { scan-assembler "r0" } } */
/* { dg-final { scan-assembler "fp" } } */
/* { dg-final { scan-assembler "fp,64" } } */
