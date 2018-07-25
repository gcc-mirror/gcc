/* This is a copy of target_attr_6.c to instead check no-strict-align.  */
/* { dg-do compile } */
/* { dg-options "-O2 -save-temps -mstrict-align" } */

/* Inlining strict-align functions into non-strict align
   functions is not allowed.  */

int
bar (int a)
{
  return a - 6;
}

__attribute__ ((target ("no-strict-align")))
int
bam (int a)
{
  return a - bar (a);
}

/* { dg-final { scan-assembler "bl.*bar" } } */
