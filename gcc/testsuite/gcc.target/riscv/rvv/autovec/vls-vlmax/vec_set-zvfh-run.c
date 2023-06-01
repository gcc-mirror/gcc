/* { dg-do run { target { riscv_zvfh_hw } } } */
/* { dg-additional-options "-march=rv64gcv_zvfh -Wno-pedantic" } */

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

#define CHECK_ALL(T)					\
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

CHECK_ALL (CHECK)

#define RUN(S, V, IDX)					\
  check_##V##_##IDX ();

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

int main ()
{
  RUN_ALL (RUN);
}
