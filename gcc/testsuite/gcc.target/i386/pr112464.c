/* { dg-do compile }  */
/* { dg-options "-Ofast -mavx512dq -ftrapv" } */

long *e;
int n, i, err;
void fn() {
  for (; i < n; i++)
    if (e[i])
      err++;
}
