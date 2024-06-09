/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -Wno-pedantic" } */

#include <assert.h>
#include <limits.h>

#include "vec_extract-1u.c"
#include "vec_extract-2u.c"
#include "vec_extract-3u.c"
#include "vec_extract-4u.c"

#define CHECK(S, V, IDX)                                                       \
  __attribute__ ((noipa, optimize ("0"))) void check_##V##_##IDX ()            \
  {                                                                            \
    V v;                                                                       \
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)                          \
      v[i] = (S) (INT_MAX - i);                                                \
    S res = vec_extract_##V##_##IDX (v);                                       \
    assert (res == (S) (INT_MAX - IDX));                                       \
  }

#define CHECK_VAR(S, V)                                                        \
  __attribute__ ((noipa, optimize ("0"))) void check_var_##V (int32_t idx)     \
  {                                                                            \
    V v;                                                                       \
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)                          \
      v[i] = (S) (INT_MAX - i);                                                \
    S res = vec_extract_var_##V (v, idx);                                      \
    assert (res == (S) (INT_MAX - idx));                                       \
  }

#define RUN(S, V, IDX) check_##V##_##IDX ();

#define RUN_VAR(S, V)                                                          \
  for (int i = 0; i < sizeof (V) / sizeof (S); i++)                            \
    check_var_##V (i);

#define RUN_ALL(T)                                                             \
  T (uint64_t, vnx2di, 0)                                                      \
  T (uint64_t, vnx2di, 1)                                                      \
  T (uint32_t, vnx4si, 0)                                                      \
  T (uint32_t, vnx4si, 1)                                                      \
  T (uint32_t, vnx4si, 3)                                                      \
  T (uint16_t, vnx8hi, 0)                                                      \
  T (uint16_t, vnx8hi, 2)                                                      \
  T (uint16_t, vnx8hi, 6)                                                      \
  T (uint8_t, vnx16qi, 0)                                                      \
  T (uint8_t, vnx16qi, 1)                                                      \
  T (uint8_t, vnx16qi, 7)                                                      \
  T (uint8_t, vnx16qi, 11)                                                     \
  T (uint8_t, vnx16qi, 15)                                                     \
  T (uint64_t, vnx4di, 0)                                                      \
  T (uint64_t, vnx4di, 1)                                                      \
  T (uint64_t, vnx4di, 2)                                                      \
  T (uint64_t, vnx4di, 3)                                                      \
  T (uint32_t, vnx8si, 0)                                                      \
  T (uint32_t, vnx8si, 1)                                                      \
  T (uint32_t, vnx8si, 3)                                                      \
  T (uint32_t, vnx8si, 4)                                                      \
  T (uint32_t, vnx8si, 7)                                                      \
  T (uint16_t, vnx16hi, 0)                                                     \
  T (uint16_t, vnx16hi, 1)                                                     \
  T (uint16_t, vnx16hi, 7)                                                     \
  T (uint16_t, vnx16hi, 8)                                                     \
  T (uint16_t, vnx16hi, 15)                                                    \
  T (uint8_t, vnx32qi, 0)                                                      \
  T (uint8_t, vnx32qi, 1)                                                      \
  T (uint8_t, vnx32qi, 15)                                                     \
  T (uint8_t, vnx32qi, 16)                                                     \
  T (uint8_t, vnx32qi, 31)                                                     \
  T (uint64_t, vnx8di, 0)                                                      \
  T (uint64_t, vnx8di, 2)                                                      \
  T (uint64_t, vnx8di, 4)                                                      \
  T (uint64_t, vnx8di, 6)                                                      \
  T (uint32_t, vnx16si, 0)                                                     \
  T (uint32_t, vnx16si, 2)                                                     \
  T (uint32_t, vnx16si, 6)                                                     \
  T (uint32_t, vnx16si, 8)                                                     \
  T (uint32_t, vnx16si, 14)                                                    \
  T (uint16_t, vnx32hi, 0)                                                     \
  T (uint16_t, vnx32hi, 2)                                                     \
  T (uint16_t, vnx32hi, 14)                                                    \
  T (uint16_t, vnx32hi, 16)                                                    \
  T (uint16_t, vnx32hi, 30)                                                    \
  T (uint8_t, vnx64qi, 0)                                                      \
  T (uint8_t, vnx64qi, 2)                                                      \
  T (uint8_t, vnx64qi, 30)                                                     \
  T (uint8_t, vnx64qi, 32)                                                     \
  T (uint8_t, vnx64qi, 63)                                                     \
  T (uint64_t, vnx16di, 0)                                                     \
  T (uint64_t, vnx16di, 4)                                                     \
  T (uint64_t, vnx16di, 8)                                                     \
  T (uint64_t, vnx16di, 12)                                                    \
  T (uint32_t, vnx32si, 0)                                                     \
  T (uint32_t, vnx32si, 4)                                                     \
  T (uint32_t, vnx32si, 12)                                                    \
  T (uint32_t, vnx32si, 16)                                                    \
  T (uint32_t, vnx32si, 28)                                                    \
  T (uint16_t, vnx64hi, 0)                                                     \
  T (uint16_t, vnx64hi, 4)                                                     \
  T (uint16_t, vnx64hi, 28)                                                    \
  T (uint16_t, vnx64hi, 32)                                                    \
  T (uint16_t, vnx64hi, 60)                                                    \
  T (uint8_t, vnx128qi, 0)                                                     \
  T (uint8_t, vnx128qi, 4)                                                     \
  T (uint8_t, vnx128qi, 30)                                                    \
  T (uint8_t, vnx128qi, 60)                                                    \
  T (uint8_t, vnx128qi, 64)                                                    \
  T (uint8_t, vnx128qi, 127)

#define RUN_ALL_VAR(T)                                                         \
  T (uint64_t, vnx2di)                                                         \
  T (uint32_t, vnx4si)                                                         \
  T (uint16_t, vnx8hi)                                                         \
  T (uint8_t, vnx16qi)                                                         \
  T (uint64_t, vnx4di)                                                         \
  T (uint32_t, vnx8si)                                                         \
  T (uint16_t, vnx16hi)                                                        \
  T (uint8_t, vnx32qi)                                                         \
  T (uint64_t, vnx8di)                                                         \
  T (uint32_t, vnx16si)                                                        \
  T (uint16_t, vnx32hi)                                                        \
  T (uint8_t, vnx64qi)                                                         \
  T (uint64_t, vnx16di)                                                        \
  T (uint32_t, vnx32si)                                                        \
  T (uint16_t, vnx64hi)                                                        \
  T (uint8_t, vnx128qi)

RUN_ALL (CHECK)
RUN_ALL_VAR (CHECK_VAR)

int
main ()
{
  RUN_ALL (RUN);
  RUN_ALL_VAR (RUN_VAR);
}
