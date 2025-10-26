/* { dg-do compile } */
/* { dg-options "-O2 -w -fisolate-erroneous-paths-dereference -mbreak-code=1" } */
/* { dg-final { scan-assembler "break\\t1" } } */

int
bug (void)
{
  return *(int *)0;
}
