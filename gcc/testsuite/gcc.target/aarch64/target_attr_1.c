/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=thunderx -march=armv8-a -dA" } */

/* Test that cpu attribute overrides the command-line -mcpu.  */

__attribute__ ((target ("cpu=cortex-a72.cortex-a53")))
int
foo (int a)
{
  return a + 1;
}

/* { dg-final { scan-assembler "//.tune cortex-a72.cortex-a53" } } */
/* { dg-final { scan-assembler-not "thunderx" } } */
