/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

/* bar has a subset set of the architectural flags of bam.
   Inlining should be allowed.  */

__attribute__ ((target ("arch=armv8-a+nocrc")))
static int
bar (int a)
{
  return a - 6;
}

__attribute__ ((target ("arch=armv8-a+crc")))
int
bam (int a)
{
  return a - bar (a);
}


/* { dg-final { scan-assembler-not "bl.*bar" } } */
