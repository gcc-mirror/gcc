/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a -mautovec-preference=asimd-only --param vect-epilogues-nomask=0" } */

/* Widening sum reductions should use the pairwise widening add and
   accumulate instructions rather than a chain of extensions feeding
   [SU]ADDW pairs.  */

#define DEF(NAME, ITYPE, OTYPE)				\
  OTYPE NAME (const ITYPE *a, long n)			\
  {							\
    OTYPE s = 0;					\
    for (long i = 0; i < n; i++)			\
      s += a[i];					\
    return s;						\
  }

DEF (sum_u8_l, unsigned char, long)
DEF (sum_i8_l, signed char, long)
DEF (sum_u16_l, unsigned short, long)
DEF (sum_i16_l, short, long)
DEF (sum_u32_l, unsigned int, long)
DEF (sum_i32_l, int, long)
DEF (sum_u8_i, unsigned char, int)
DEF (sum_i8_i, signed char, int)
DEF (sum_u16_i, unsigned short, int)
DEF (sum_i16_i, short, int)

/* { dg-final { scan-assembler-times {\tuaddlp\tv[0-9]+\.8h, v[0-9]+\.16b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsaddlp\tv[0-9]+\.8h, v[0-9]+\.16b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuaddlp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsaddlp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tv[0-9]+\.2d, v[0-9]+\.4s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsadalp\tv[0-9]+\.2d, v[0-9]+\.4s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsadalp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 2 } } */

/* { dg-final { scan-assembler-not {\tuaddw2?\t} } } */
/* { dg-final { scan-assembler-not {\tsaddw2?\t} } } */
/* { dg-final { scan-assembler-not {\tzip1\t} } } */
