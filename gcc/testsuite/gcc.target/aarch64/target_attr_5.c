/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

/* Make sure that bar is inlined into bam.  */

__attribute__ ((target ("arch=armv8-a+nocrc")))
static int
bar (int a)
{
  return a - 6;
}

__attribute__ ((target ("cpu=cortex-a53+nocrc")))
int
bam (int a)
{
  return a - bar (a);
}

/* { dg-final { scan-assembler-not "bl.*bar" } } */
