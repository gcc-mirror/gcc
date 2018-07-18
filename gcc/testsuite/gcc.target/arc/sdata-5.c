/* { dg-do compile } */
/* { dg-options "-Os -msdata" } */

/* Check interaction between section anchors and small data. */

const int a[1] = {};
static short b[] = {};

int c;

const int* fn1 (void)
{
  return a + b[c];
}
/* { dg-final { scan-assembler "@c@sda" } } */
