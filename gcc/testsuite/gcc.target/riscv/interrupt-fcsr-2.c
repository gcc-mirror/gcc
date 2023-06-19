/* Verify that fcsr instructions emitted.  */
/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O" } */

extern int foo (void);
extern float interrupt_count;
void __attribute__ ((interrupt))
sub (void)
{
  interrupt_count++;
}

/* { dg-final { scan-assembler-times "frcsr\t" 1 } } */
/* { dg-final { scan-assembler-times "fscsr\t" 1 } } */
