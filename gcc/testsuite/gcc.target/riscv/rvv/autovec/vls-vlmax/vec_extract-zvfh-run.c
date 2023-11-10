/* { dg-do run {target { riscv_zvfh } } } */
/* { dg-additional-options "-std=gnu99 -Wno-pedantic" } */

#include <assert.h>

#include "vec_extract-1.c"
#include "vec_extract-2.c"
#include "vec_extract-3.c"
#include "vec_extract-4.c"

#define CHECK(S, V, IDX)				\
void check_##V##_##IDX ()				\
  {							\
    V v;						\
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
      v[i] = i;						\
    S res = vec_extract_##V##_##IDX (v);		\
    assert (res == v[IDX]);				\
  }

#define CHECK_VAR(S, V)					\
__attribute__ ((noipa))					\
void check_var_##V (int32_t idx)			\
  {							\
    V v;						\
    for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
      v[i] = i;						\
    S res = vec_extract_var_##V (v, idx);		\
    assert (res == v[idx]);				\
  }

#define RUN(S, V, IDX)					\
  check_##V##_##IDX ();

#define RUN_VAR(S, V)					\
  for (int i = 0; i < sizeof (V) / sizeof (S); i++)	\
    check_var_##V (i);					\

#define RUN_ALL(T)					\
  T (_Float16, vnx8hf, 0)				\
  T (_Float16, vnx8hf, 3)				\
  T (_Float16, vnx8hf, 7)				\
  T (_Float16, vnx16hf, 0)				\
  T (_Float16, vnx16hf, 3)				\
  T (_Float16, vnx16hf, 7)				\
  T (_Float16, vnx16hf, 8)				\
  T (_Float16, vnx16hf, 15)				\
  T (_Float16, vnx32hf, 0)				\
  T (_Float16, vnx32hf, 3)				\
  T (_Float16, vnx32hf, 7)				\
  T (_Float16, vnx32hf, 8)				\
  T (_Float16, vnx32hf, 16)				\
  T (_Float16, vnx32hf, 31)				\
  T (_Float16, vnx64hf, 0)				\
  T (_Float16, vnx64hf, 3)				\
  T (_Float16, vnx64hf, 7)				\
  T (_Float16, vnx64hf, 8)				\
  T (_Float16, vnx64hf, 16)				\
  T (_Float16, vnx64hf, 31)				\
  T (_Float16, vnx64hf, 42)				\
  T (_Float16, vnx64hf, 63)				\

#define RUN_ALL_VAR(T)					\
  T (_Float16, vnx8hf)					\
  T (_Float16, vnx16hf)					\
  T (_Float16, vnx32hf)					\
  T (_Float16, vnx64hf)					\

RUN_ALL (CHECK)
RUN_ALL_VAR (CHECK_VAR)

int main ()
{
  RUN_ALL (RUN);
  RUN_ALL_VAR (RUN_VAR)
}
