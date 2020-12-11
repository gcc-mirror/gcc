/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_condition } */

_Bool arr[16];

void foo(char *q)
{
  char *p = __builtin_assume_aligned (q, 16);
  _Bool b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15;
  b0 = p[0] != 0;
  b1 = p[1] != 0;
  b2 = p[2] != 0;
  b3 = p[3] != 0;
  b4 = p[4] != 0;
  b5 = p[5] != 0;
  b6 = p[6] != 0;
  b7 = p[7] != 0;
  b8 = p[8] != 0;
  b9 = p[9] != 0;
  b10 = p[10] != 0;
  b11 = p[11] != 0;
  b12 = p[12] != 0;
  b13 = p[13] != 0;
  b14 = p[14] != 0;
  b15 = p[15] != 0;
  arr[0] = b0;
  arr[1] = b1;
  arr[2] = b2;
  arr[3] = b3;
  arr[4] = b4;
  arr[5] = b5;
  arr[6] = b6;
  arr[7] = b7;
  arr[8] = b8;
  arr[9] = b9;
  arr[10] = b10;
  arr[11] = b11;
  arr[12] = b12;
  arr[13] = b13;
  arr[14] = b14;
  arr[15] = b15;
}

/* { dg-final { scan-tree-dump "transform load" "slp2" } } */
/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
