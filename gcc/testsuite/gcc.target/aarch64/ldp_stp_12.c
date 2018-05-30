/* { dg-options "-O2" } */

void
store_offset (int *array, int x, int y)
{
  array[1085] = x;
  array[1084] = y;

  array[1086] = y;
  array[1087] = 5;
}

/* { dg-final { scan-assembler-times "stp\tw\[0-9\]+, w\[0-9\]+, " 2 } } */
