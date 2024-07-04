/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -Wno-pedantic" } */

#include <assert.h>

#include "vec_set-1.c"
#include "vec_set-2.c"
#include "vec_set-3.c"
#include "vec_set-4.c"

#define CHECK(S, V, IDX)				\
void check_##V##_##IDX ()				\
  {							\
    V v;						\
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
      v[i] = i;						\
    V res = vec_set_##V##_##IDX (v, 77);		\
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
      assert (res[i] == (i == IDX ? 77 : i));		\
  }

#define CHECK_VAR(S, V)					\
__attribute__ ((noipa))					\
void check_var_##V (int32_t idx)			\
  {							\
    V v;						\
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
      v[i] = i;						\
    V res = vec_set_var_##V (v, idx, 77);		\
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
      assert (res[i] == (i == idx ? 77 : i));		\
  }

#define RUN(S, V, IDX)					\
  check_##V##_##IDX ();					\

#define RUN_VAR(S, V)					\
  for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
    check_var_##V (i);					\

#define RUN_ALL(T)					\
  T (float, vnx4sf, 0)					\
  T (float, vnx4sf, 1)					\
  T (float, vnx4sf, 3)					\
  T (double, vnx2df, 0)					\
  T (double, vnx2df, 1)					\
  T (int64_t, vnx2di, 0)				\
  T (int64_t, vnx2di, 1)				\
  T (int32_t, vnx4si, 0)				\
  T (int32_t, vnx4si, 1)				\
  T (int32_t, vnx4si, 3)				\
  T (int16_t, vnx8hi, 0)				\
  T (int16_t, vnx8hi, 2)				\
  T (int16_t, vnx8hi, 6)				\
  T (int8_t, vnx16qi, 0)				\
  T (int8_t, vnx16qi, 1)				\
  T (int8_t, vnx16qi, 7)				\
  T (int8_t, vnx16qi, 11)				\
  T (int8_t, vnx16qi, 15)				\
  T (float, vnx8sf, 0)					\
  T (float, vnx8sf, 1)					\
  T (float, vnx8sf, 3)					\
  T (float, vnx8sf, 4)					\
  T (float, vnx8sf, 7)					\
  T (double, vnx4df, 0)					\
  T (double, vnx4df, 1)					\
  T (double, vnx4df, 2)					\
  T (double, vnx4df, 3)					\
  T (int64_t, vnx4di, 0)				\
  T (int64_t, vnx4di, 1)				\
  T (int64_t, vnx4di, 2)				\
  T (int64_t, vnx4di, 3)				\
  T (int32_t, vnx8si, 0)				\
  T (int32_t, vnx8si, 1)				\
  T (int32_t, vnx8si, 3)				\
  T (int32_t, vnx8si, 4)				\
  T (int32_t, vnx8si, 7)				\
  T (int16_t, vnx16hi, 0)				\
  T (int16_t, vnx16hi, 1)				\
  T (int16_t, vnx16hi, 7)				\
  T (int16_t, vnx16hi, 8)				\
  T (int16_t, vnx16hi, 15)				\
  T (int8_t, vnx32qi, 0)				\
  T (int8_t, vnx32qi, 1)				\
  T (int8_t, vnx32qi, 15)				\
  T (int8_t, vnx32qi, 16)				\
  T (int8_t, vnx32qi, 31)				\
  T (float, vnx16sf, 0)					\
  T (float, vnx16sf, 2)					\
  T (float, vnx16sf, 6)					\
  T (float, vnx16sf, 8)					\
  T (float, vnx16sf, 14)				\
  T (double, vnx8df, 0)					\
  T (double, vnx8df, 2)					\
  T (double, vnx8df, 4)					\
  T (double, vnx8df, 6)					\
  T (int64_t, vnx8di, 0)				\
  T (int64_t, vnx8di, 2)				\
  T (int64_t, vnx8di, 4)				\
  T (int64_t, vnx8di, 6)				\
  T (int32_t, vnx16si, 0)				\
  T (int32_t, vnx16si, 2)				\
  T (int32_t, vnx16si, 6)				\
  T (int32_t, vnx16si, 8)				\
  T (int32_t, vnx16si, 14)				\
  T (int16_t, vnx32hi, 0)				\
  T (int16_t, vnx32hi, 2)				\
  T (int16_t, vnx32hi, 14)				\
  T (int16_t, vnx32hi, 16)				\
  T (int16_t, vnx32hi, 30)				\
  T (int8_t, vnx64qi, 0)				\
  T (int8_t, vnx64qi, 2)				\
  T (int8_t, vnx64qi, 30)				\
  T (int8_t, vnx64qi, 32)				\
  T (int8_t, vnx64qi, 63)				\
  T (float, vnx32sf, 0)					\
  T (float, vnx32sf, 3)					\
  T (float, vnx32sf, 12)				\
  T (float, vnx32sf, 17)				\
  T (float, vnx32sf, 14)				\
  T (double, vnx16df, 0)				\
  T (double, vnx16df, 4)				\
  T (double, vnx16df, 8)				\
  T (double, vnx16df, 12)				\
  T (int64_t, vnx16di, 0)				\
  T (int64_t, vnx16di, 4)				\
  T (int64_t, vnx16di, 8)				\
  T (int64_t, vnx16di, 12)				\
  T (int32_t, vnx32si, 0)				\
  T (int32_t, vnx32si, 4)				\
  T (int32_t, vnx32si, 12)				\
  T (int32_t, vnx32si, 16)				\
  T (int32_t, vnx32si, 28)				\
  T (int16_t, vnx64hi, 0)				\
  T (int16_t, vnx64hi, 4)				\
  T (int16_t, vnx64hi, 28)				\
  T (int16_t, vnx64hi, 32)				\
  T (int16_t, vnx64hi, 60)				\
  T (int8_t, vnx128qi, 0)				\
  T (int8_t, vnx128qi, 4)				\
  T (int8_t, vnx128qi, 30)				\
  T (int8_t, vnx128qi, 60)				\
  T (int8_t, vnx128qi, 64)				\
  T (int8_t, vnx128qi, 127)				\

#define RUN_ALL_VAR(T)					\
  T (float, vnx4sf)					\
  T (double, vnx2df)					\
  T (int64_t, vnx2di)					\
  T (int32_t, vnx4si)					\
  T (int16_t, vnx8hi)					\
  T (int8_t, vnx16qi)					\
  T (float, vnx8sf)					\
  T (double, vnx4df)					\
  T (int64_t, vnx4di)					\
  T (int32_t, vnx8si)					\
  T (int16_t, vnx16hi)					\
  T (int8_t, vnx32qi)					\
  T (float, vnx16sf)					\
  T (double, vnx8df)					\
  T (int64_t, vnx8di)					\
  T (int32_t, vnx16si)					\
  T (int16_t, vnx32hi)					\
  T (int8_t, vnx64qi)					\
  T (float, vnx32sf)					\
  T (double, vnx16df)					\
  T (int64_t, vnx16di)					\
  T (int32_t, vnx32si)					\
  T (int16_t, vnx64hi)					\
  T (int8_t, vnx128qi)					\

RUN_ALL (CHECK)
RUN_ALL_VAR (CHECK_VAR)

int main ()
{
  RUN_ALL (RUN);
  RUN_ALL_VAR (RUN_VAR);
}
