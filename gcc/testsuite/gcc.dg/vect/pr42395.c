/* PR debug/42395 */
/* { dg-do compile } */
/* { dg-additional-options "-O3 -g" } */

void foo(int j, int *A)
{
  int i;
  for (i = 0; i < j; i ++) A[i] = i;
  for (; i < 4096; i ++) A[i] = 0;
}
