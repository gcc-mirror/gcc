/* { dg-do compile } */
/* { dg-options "-Og -march=x86-64 -mavx512vl -fsanitize=thread -fstack-protector-all" } */

typedef char __attribute__((__vector_size__(8))) C;
typedef int __attribute__((__vector_size__(8))) U;
typedef int __attribute__((__vector_size__(16))) V;
typedef int __attribute__((__vector_size__(32))) W;
typedef long long __attribute__((__vector_size__(64))) L;
typedef _Float64 __attribute__((__vector_size__(16))) F;
typedef _Float64 __attribute__((__vector_size__(64))) G;
C c;
int i;

U foo0( W v256u32_0,
           W v256s32_0,
           V v128u64_0,
           V v128s64_0,
           W v256u64_0,
           W v256s64_0,
           L v512s64_0,
           W v256u128_0,
           W v256s128_0,
           V v128f32_0,
           W v256f32_0,
           F F_0,
           W v256f64_0,
           G G_0) {
  C U_1 = __builtin_ia32_pshufb(c, c);
  G_0 += __builtin_convertvector(v512s64_0, G);
  F F_1 = __builtin_shufflevector(F_0, G_0, 2, 2);
  W W_r = v256u32_0 + v256s32_0 + v256u64_0 + v256s64_0 + v256u128_0 +
                    v256s128_0 + v256f32_0 + v256f64_0;
  V V_r = ((union {
                      W a;
                      V b;
                    })W_r)
                        .b +
                    i + v128u64_0 + v128s64_0 + v128f32_0 +
                    (V)F_1;
  U U_r = ((union {
                    V a;
                    U b;
                  })V_r)
                      .b +
                  (U)U_1;
  return U_r;
}
