/* { dg-do compile } */
/* { dg-additional-options "-O -ftree-vectorize" } */

void f(int n, int y, char *arr_2, char *arr_6) {
  for (int i = y; i < n; i++)
    arr_6[i] = arr_6[i] ? (arr_2[i] ? 3 : 8) : 1;
}

/* { dg-final { scan-assembler-not {\tand\tp[0-9]+.b} } } */
