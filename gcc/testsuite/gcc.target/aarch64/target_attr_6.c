/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

/* Inlining strict-align functions into non-strict align
   functions is not allowed.  */

__attribute__ ((target ("strict-align")))
int
bar (int a)
{
  return a - 6;
}

int
bam (int a)
{
  return a - bar (a);
}

/* { dg-final { scan-assembler "bl.*bar" } } */
