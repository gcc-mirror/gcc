/* { dg-options "-O2 -moverride=tune=none" } */

typedef float float32x4_t __attribute__ ((__vector_size__ ((16))));

float32x4_t arr[4][4];

void
foo (float32x4_t x, float32x4_t y)
{
  arr[0][1] = x;
  arr[1][0] = y;
  arr[2][0] = x;
  arr[1][1] = y;
  arr[0][2] = x;
  arr[0][3] = y;
  arr[1][2] = x;
  arr[2][1] = y;
  arr[3][0] = x;
  arr[3][1] = y;
  arr[2][2] = x;
  arr[1][3] = y;
  arr[2][3] = x;
  arr[3][2] = y;
}

/* { dg-final { scan-assembler-times "stp\tq\[0-9\]+, q\[0-9\]" 7 } } */
