// PR libstdc++/81706
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-O3 -mavx2 -mno-avx512f" }
// { dg-final { scan-assembler "call\[^\n\r]__?ZGVdN4v_cos" } }
// { dg-final { scan-assembler "call\[^\n\r]__?ZGVdN4v_sin" } }

#ifdef __cplusplus
extern "C" {
#endif
extern double cos (double) __attribute__ ((nothrow, leaf, simd ("notinbranch")));
extern double sin (double) __attribute__ ((nothrow, leaf, simd ("notinbranch")));
#ifdef __cplusplus
}
#endif
double p[1024] = { 1.0 };
double q[1024] = { 1.0 };

void
foo (void)
{
  int i;
  for (i = 0; i < 1024; i++)
    p[i] = cos (q[i]);
}

void
bar (void)
{
  int i;
  for (i = 0; i < 1024; i++)
    p[i] = __builtin_sin (q[i]);
}
