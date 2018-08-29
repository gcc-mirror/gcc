/* Check if sdata access is done correctly, specially
   for variables which are having a different alignment
   than the default data type indicates.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

short gA  __attribute__ ((aligned(1)));

void foo (void)
{
  gA += gA + 3;
}

/* { dg-final { scan-assembler-not "ld\[wh\]_s r0,\\\[gp" } } */
/* { dg-final { scan-assembler-not "st\[wh\]\\\.as.*gp" } } */
