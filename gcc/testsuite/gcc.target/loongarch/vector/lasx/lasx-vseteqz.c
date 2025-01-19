/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */
/* { dg-final { scan-assembler "\txvset.*.v\t" } } */
/* { dg-final { scan-assembler  "bcnez" } } */

int
foo (int N)
{
  for (int i = 0; i <= N; i++)
    if (i * i == N)
      return i;
  return -1;
}

