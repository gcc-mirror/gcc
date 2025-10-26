/* { dg-do compile } */
/* { dg-options "-O2 -w -fisolate-erroneous-paths-dereference" } */
/* { dg-final { scan-assembler "amswap\\.w\\t\\\$r0,\\\$r1,\\\$r0" } } */

int
bug (void)
{
  return *(int *)0;
}
