/* Check that the decrement-and-test instruction is generated.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-times "dt\tr" 2 } } */

int
test_00 (int* x, int c)
{
  int s = 0;
  int i;
  for (i = 0; i < c; ++i)
    s += x[i];
  return s;
}

int
test_01 (int* x, int c)
{
  int s = 0;
  int i;
  for (i = 0; i < c; ++i)
    s += *--x;
  return s;
}
