/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int *foo;

int
bar (int x)
{
  return foo[x];
}
/* { dg-final { scan-assembler-not "lea\[lq\]" } } */
