/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -mautovec-preference=asimd-only --param vect-epilogues-nomask=0" } */

/* With dot product every widening sum that passes through a byte to word
   step uses one [SU]DOT for that step.  A step that starts or ends
   somewhere else still uses the pairwise widening instructions.  */

#define DEF(NAME, ITYPE, OTYPE)				\
  OTYPE NAME (const ITYPE *a, long n)			\
  {							\
    OTYPE s = 0;					\
    for (long i = 0; i < n; i++)			\
      s += a[i];					\
    return s;						\
  }

/* 2x, no dot product: the result elements are too narrow.  */
DEF (sum_u8_h, unsigned char, unsigned short)
DEF (sum_i8_h, signed char, short)
DEF (sum_u16_i, unsigned short, int)
DEF (sum_i16_i, short, int)
DEF (sum_u32_l, unsigned int, long)
DEF (sum_i32_l, int, long)

/* 4x from bytes: one dot product.  */
DEF (sum_u8_i, unsigned char, int)
DEF (sum_i8_i, signed char, int)

/* 4x from halfwords: no dot product for that element size.  */
DEF (sum_u16_l, unsigned short, long)
DEF (sum_i16_l, short, long)

/* 8x from bytes: a dot product followed by one pairwise accumulate.  */
DEF (sum_u8_l, unsigned char, long)
DEF (sum_i8_l, signed char, long)

/* { dg-final { scan-assembler-times {\tudot\tv[0-9]+\.4s, v[0-9]+\.16b, v[0-9]+\.16b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsdot\tv[0-9]+\.4s, v[0-9]+\.16b, v[0-9]+\.16b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tv[0-9]+\.8h, v[0-9]+\.16b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsadalp\tv[0-9]+\.8h, v[0-9]+\.16b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsadalp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuaddlp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsaddlp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tv[0-9]+\.2d, v[0-9]+\.4s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsadalp\tv[0-9]+\.2d, v[0-9]+\.4s\n} 3 } } */

/* The byte to halfword step is what the dot product replaces.  */
/* { dg-final { scan-assembler-not {\t[su]addlp\tv[0-9]+\.8h, v[0-9]+\.16b\n} } } */
/* { dg-final { scan-assembler-not {\t[su]addw2?\t} } } */
