/* PR target/102464.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -fno-trapping-math" } */

#define FOO(FUNC,SUFFIX)                       \
  _Float16                                     \
  foo_##FUNC##_##SUFFIX (_Float16 a)           \
  {                                            \
    return __builtin_##FUNC##SUFFIX (a);       \
  }

FOO (roundeven, f16);
FOO (roundeven, f);
FOO (roundeven, );
FOO (roundeven, l);
FOO (trunc, f16);
FOO (trunc, f);
FOO (trunc, );
FOO (trunc, l);
FOO (ceil, f16);
FOO (ceil, f);
FOO (ceil, );
FOO (ceil, l);
FOO (floor, f16);
FOO (floor, f);
FOO (floor, );
FOO (floor, l);
FOO (nearbyint, f16);
FOO (nearbyint, f);
FOO (nearbyint, );
FOO (nearbyint, l);
FOO (rint, f16);
FOO (rint, f);
FOO (rint, );
FOO (rint, l);

/* { dg-final { scan-assembler-not "vcvtsh2s\[sd\]" } } */
/* { dg-final { scan-assembler-not "extendhfxf" } } */
/* { dg-final { scan-assembler-times "vrndscalesh\[^\n\r\]*xmm\[0-9\]" 24 } } */
