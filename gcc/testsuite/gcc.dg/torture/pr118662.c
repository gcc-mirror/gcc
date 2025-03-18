/* { dg-do run } */
/* { dg-additional-options "-ftree-slp-vectorize -fno-vect-cost-model" } */
/* { dg-additional-options "-msse4" { target sse4_runtime} } */

int __attribute__((noipa)) addup(signed char *num) {
  int val = num[0] + num[1] + num[2] + num[3];
  if (num[3] >= 0)
    val++;
  return val;
}

int main(int, char *[])
{
  signed char num[4] = {1, 1, 1, -1};
  if (addup(num) != 2)
    __builtin_abort();
  return 0;
}
