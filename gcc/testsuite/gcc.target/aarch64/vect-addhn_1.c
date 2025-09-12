/* { dg-require-effective-target vect_int } */
/* { dg-options "-O3 -std=c99" } */

#include <stdint.h>
#include <stdio.h>

#define N 1000
#define CHECK_ERROR(cond, fmt, ...) \
  do { if (cond) { printf(fmt "\n", ##__VA_ARGS__); __builtin_abort (); } } while (0)

// Generates all test components for a given type combo
#define TEST_COMBO(A_TYPE, C_TYPE, CAST_TYPE, SHIFT)                                  \
  A_TYPE a_##A_TYPE##_##C_TYPE[N];                                                    \
  A_TYPE b_##A_TYPE##_##C_TYPE[N];                                                    \
  C_TYPE c_##A_TYPE##_##C_TYPE[N];                                                    \
  C_TYPE ref_##A_TYPE##_##C_TYPE[N];                                                  \
                                                                                      \
  void init_##A_TYPE##_##C_TYPE() {                                                   \
    _Pragma ("GCC novector")							      \
    for (int i = 0; i < N; i++) {                                                     \
      a_##A_TYPE##_##C_TYPE[i] = (A_TYPE)(i * 3);                                     \
      b_##A_TYPE##_##C_TYPE[i] = (A_TYPE)(i * 7);                                     \
    }                                                                                 \
  }                                                                                   \
                                                                                      \
  void foo_##A_TYPE##_##C_TYPE() {                                                    \
    for (int i = 0; i < N; i++)                                                       \
      c_##A_TYPE##_##C_TYPE[i] =                                                      \
        ((CAST_TYPE)a_##A_TYPE##_##C_TYPE[i] +                                        \
         (CAST_TYPE)b_##A_TYPE##_##C_TYPE[i]) >> SHIFT;                               \
  }                                                                                   \
                                                                                      \
  void ref_##A_TYPE##_##C_TYPE##_compute() {                                          \
    _Pragma ("GCC novector")							      \
    for (int i = 0; i < N; i++)                                                       \
      ref_##A_TYPE##_##C_TYPE[i] =                                                    \
        ((CAST_TYPE)a_##A_TYPE##_##C_TYPE[i] +                                        \
         (CAST_TYPE)b_##A_TYPE##_##C_TYPE[i]) >> SHIFT;                               \
  }                                                                                   \
                                                                                      \
  void validate_##A_TYPE##_##C_TYPE(const char* variant_name) {                       \
    _Pragma ("GCC novector")							      \
    for (int i = 0; i < N; i++) {                                                     \
      if (c_##A_TYPE##_##C_TYPE[i] != ref_##A_TYPE##_##C_TYPE[i]) {                   \
        printf("FAIL [%s]: Index %d: got %lld, expected %lld\n",                      \
               variant_name, i,                                                       \
               (long long)c_##A_TYPE##_##C_TYPE[i],                                   \
               (long long)ref_##A_TYPE##_##C_TYPE[i]);                                \
        __builtin_abort ();                                                           \
      }                                                                               \
    }                                                                                 \
  }

// Runs the test for one combo with name output
#define RUN_COMBO(A_TYPE, C_TYPE)                          \
  do {                                                     \
    init_##A_TYPE##_##C_TYPE();                            \
    foo_##A_TYPE##_##C_TYPE();                             \
    ref_##A_TYPE##_##C_TYPE##_compute();                   \
    validate_##A_TYPE##_##C_TYPE(#A_TYPE " -> " #C_TYPE);  \
  } while (0)

// Instantiate all valid combinations
TEST_COMBO(int16_t, int8_t, int32_t, 8)
TEST_COMBO(uint16_t, uint8_t, uint32_t, 8)
TEST_COMBO(int32_t, int16_t, int64_t, 16)
TEST_COMBO(uint32_t, uint16_t, uint64_t, 16)
#if defined(__aarch64__)
TEST_COMBO(int64_t, int32_t, __int128_t, 32)
TEST_COMBO(uint64_t, uint32_t, unsigned __int128, 32)
#endif

int main() {

  RUN_COMBO(int16_t, int8_t);
  RUN_COMBO(uint16_t, uint8_t);
  RUN_COMBO(int32_t, int16_t);
  RUN_COMBO(uint32_t, uint16_t);
#if defined(__aarch64__)
  RUN_COMBO(int64_t, int32_t);
  RUN_COMBO(uint64_t, uint32_t);
#endif

  return 0;
}

