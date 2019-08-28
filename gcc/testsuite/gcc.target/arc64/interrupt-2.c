/* Verify that arg regs used as temporaries get saved.  */
/* { dg-do compile } */
/* { dg-options "-O" } */
void __attribute__ ((interrupt))
foo2 (void)
{
  extern volatile int INTERRUPT_FLAG;
  INTERRUPT_FLAG = 0;

  extern volatile int COUNTER;
  COUNTER++;
}
/* { dg-final { scan-assembler "push.*r0" { target { hs6x } } } } */
/* { dg-final { scan-assembler "pop.*r0" { target { hs6x } } } } */

/* { dg-final { scan-assembler "st\.a.*r0" { target { hs5x } } } } */
/* { dg-final { scan-assembler "ld\.ab.*r0" { target { hs5x } } } } */
