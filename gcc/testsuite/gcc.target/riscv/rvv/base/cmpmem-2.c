/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O2 -mrvv-max-lmul=dynamic" } */
/* { dg-add-options riscv_v } */

#include <stdlib.h>

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

static inline __attribute__ ((always_inline)) void
do_one_test (int const size, int const diff_offset, int const diff_dir)
{
  unsigned char A[size];
  unsigned char B[size];
  unsigned char const fill_value = 0x55;
  __builtin_memset (A, fill_value, size);
  __builtin_memset (B, fill_value, size);

  if (diff_dir != 0)
    {
      if (diff_dir < 0)
        {
          A[diff_offset] = fill_value - 1;
        }
      else
        {
          A[diff_offset] = fill_value + 1;
        }
    }

  if (__builtin_memcmp (A, B, size) != diff_dir)
    {
      abort ();
    }
}

int
main ()
{
  do_one_test (0, 0, 0);

  do_one_test (1, 0, -1);
  do_one_test (1, 0, 0);
  do_one_test (1, 0, 1);

  do_one_test (MIN_VECTOR_BYTES - 1, 0, -1);
  do_one_test (MIN_VECTOR_BYTES - 1, 0, 0);
  do_one_test (MIN_VECTOR_BYTES - 1, 0, 1);
  do_one_test (MIN_VECTOR_BYTES - 1, 1, -1);
  do_one_test (MIN_VECTOR_BYTES - 1, 1, 0);
  do_one_test (MIN_VECTOR_BYTES - 1, 1, 1);

  do_one_test (MIN_VECTOR_BYTES, 0, -1);
  do_one_test (MIN_VECTOR_BYTES, 0, 0);
  do_one_test (MIN_VECTOR_BYTES, 0, 1);
  do_one_test (MIN_VECTOR_BYTES, MIN_VECTOR_BYTES - 1, -1);
  do_one_test (MIN_VECTOR_BYTES, MIN_VECTOR_BYTES - 1, 0);
  do_one_test (MIN_VECTOR_BYTES, MIN_VECTOR_BYTES - 1, 1);

  do_one_test (MIN_VECTOR_BYTES + 1, 0, -1);
  do_one_test (MIN_VECTOR_BYTES + 1, 0, 0);
  do_one_test (MIN_VECTOR_BYTES + 1, 0, 1);
  do_one_test (MIN_VECTOR_BYTES + 1, MIN_VECTOR_BYTES, -1);
  do_one_test (MIN_VECTOR_BYTES + 1, MIN_VECTOR_BYTES, 0);
  do_one_test (MIN_VECTOR_BYTES + 1, MIN_VECTOR_BYTES, 1);

  do_one_test (MIN_VECTOR_BYTES * 8, 0, -1);
  do_one_test (MIN_VECTOR_BYTES * 8, 0, 0);
  do_one_test (MIN_VECTOR_BYTES * 8, 0, 1);
  do_one_test (MIN_VECTOR_BYTES * 8, MIN_VECTOR_BYTES * 8 - 1, -1);
  do_one_test (MIN_VECTOR_BYTES * 8, MIN_VECTOR_BYTES * 8 - 1, 0);
  do_one_test (MIN_VECTOR_BYTES * 8, MIN_VECTOR_BYTES * 8 - 1, 1);

  return 0;
}
