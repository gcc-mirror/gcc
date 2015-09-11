/* { dg-do compile } */
/* { dg-options "-O2 -save-temps" } */

/* Inlining non-strict-align functions into strict-align
   functions is allowed.  */

static int
bar (int a)
{
  return a - 6;
}

__attribute__ ((target ("strict-align")))
int
bam (int a)
{
  return a - bar (a);
}

/* { dg-final { scan-assembler-not "bl.*bar" } } */
