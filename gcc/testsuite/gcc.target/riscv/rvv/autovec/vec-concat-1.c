/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcv_zvfhmin_zvfbfmin -mabi=lp64d -mrvv-vector-bits=scalable -mrvv-max-lmul=m8" } */

/* Test VLS vector concatenation via shufflevector across all data types.  */

#define INDICES_2(x) (x), (x + 1)
#define INDICES_4(x) INDICES_2 (x), INDICES_2 (x + 2)
#define INDICES_8(x) INDICES_4 (x), INDICES_4 (x + 4)
#define INDICES_16(x) INDICES_8 (x), INDICES_8 (x + 8)
#define INDICES_32(x) INDICES_16 (x), INDICES_16 (x + 16)
#define INDICES_64(x) INDICES_32 (x), INDICES_32 (x + 32)

#define TEST_MODES(T) \
  T (signed char, qi, 2) \
  T (signed char, qi, 4) \
  T (signed char, qi, 8) \
  T (signed char, qi, 16) \
  T (signed char, qi, 32) \
  T (signed char, qi, 64) \
  T (short, hi, 2) \
  T (short, hi, 4) \
  T (short, hi, 8) \
  T (short, hi, 16) \
  T (short, hi, 32) \
  T (int, si, 2) \
  T (int, si, 4) \
  T (int, si, 8) \
  T (int, si, 16) \
  T (long long, di, 2) \
  T (long long, di, 4) \
  T (long long, di, 8) \
  T (float, sf, 2) \
  T (float, sf, 4) \
  T (float, sf, 8) \
  T (float, sf, 16) \
  T (double, df, 2) \
  T (double, df, 4) \
  T (double, df, 8) \
  T (_Float16, hf, 2) \
  T (_Float16, hf, 4) \
  T (_Float16, hf, 8) \
  T (_Float16, hf, 16) \
  T (_Float16, hf, 32) \
  T (__bf16, bf, 2) \
  T (__bf16, bf, 4) \
  T (__bf16, bf, 8) \
  T (__bf16, bf, 16) \
  T (__bf16, bf, 32)

#define DEF_CONCAT(TYPE, ID, N) \
  typedef TYPE half_##ID##N \
    __attribute__ ((vector_size (N * sizeof (TYPE)))); \
  typedef TYPE full_##ID##N \
    __attribute__ ((vector_size (2 * N * sizeof (TYPE)))); \
  __attribute__ ((noipa)) full_##ID##N \
  concat_##ID##N (half_##ID##N a, half_##ID##N b) \
  { \
    return __builtin_shufflevector (a, b, INDICES_##N (0), INDICES_##N (N)); \
  }

TEST_MODES (DEF_CONCAT)

/* LMUL <= 1: 11 funcs x 3 slideups (using vec_concat, from spills + 1 slideup
				     to no spills + 3 slideups).
   LMUL > 1:  24 funcs x 1 slideup  (no vec_concat, using fallback instead,
				     nothing changed from trunk).  */
/* { dg-final { scan-assembler-times "vslideup" 57 } } */
/* { dg-final { scan-assembler-not "vslide1down" } } */
