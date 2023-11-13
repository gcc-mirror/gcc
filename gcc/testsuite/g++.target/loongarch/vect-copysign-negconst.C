/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mlasx -mno-strict-align" } */
/* { dg-final { scan-assembler "\txvbitseti.*63" } } */
/* { dg-final { scan-assembler "\txvbitseti.*31" } } */
/* { dg-final { scan-assembler "\tvbitseti.*63" } } */
/* { dg-final { scan-assembler "\tvbitseti.*31" } } */

template <int N>
__attribute__ ((noipa)) void
force_negative (float *arr)
{
  for (int i = 0; i < N; i++)
    arr[i] = __builtin_copysignf (arr[i], -2);
}

template <int N>
__attribute__ ((noipa)) void
force_negative (double *arr)
{
  for (int i = 0; i < N; i++)
    arr[i] = __builtin_copysign (arr[i], -3);
}

template void force_negative<4>(float *);
template void force_negative<8>(float *);
template void force_negative<2>(double *);
template void force_negative<4>(double *);
