/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

/* bar does not have a subset of architectural flags of bam.
   Inlining should be rejected.  */

__attribute__ ((target ("arch=armv8-a+crc")))
int
bar (int a)
{
  return a - 6;
}

__attribute__ ((target ("arch=armv8-a+nocrc")))
int
bam (int a)
{
  return a - bar (a);
}


/* { dg-final { scan-assembler "bl.*bar" } } */
