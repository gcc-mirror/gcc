/* { dg-options "-O2 -mstrict-align" } */

double arr[4][4];

void
foo ()
{
  arr[0][1] = 1;
  arr[1][0] = -1;
  arr[2][0] = 1;
  arr[1][1] = -1;
  arr[0][2] = 1;
  arr[0][3] = -1;
  arr[1][2] = 1;
  arr[2][1] = -1;
  arr[3][0] = 1;
  arr[3][1] = -1;
  arr[2][2] = 1;
  arr[1][3] = -1;
  arr[2][3] = 1;
  arr[3][2] = -1;
}

/* { dg-final { scan-assembler-times "stp\td\[0-9\]+, d\[0-9\]" 7 } } */
