/* { dg-do run { target { rv64 && riscv_zbc_ok } } } */
/* { dg-options "-march=rv64gc_zbc -O2 -save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" "-Os" "-Oz" } } */

#include <stdint-gcc.h>

#define CRC64_POLY 0x42f0e1eba9ea3693ull

__attribute__ ((target ("arch=rv64gc"), noinline))
int64_t
rev_crc64_ref (int64_t crc, int64_t data)
{
  return __builtin_rev_crc64_data64 (crc, data, CRC64_POLY);
}

__attribute__ ((noinline))
int64_t
rev_crc64_fast (int64_t crc, int64_t data)
{
  return __builtin_rev_crc64_data64 (crc, data, CRC64_POLY);
}

int
main (void)
{
  int64_t crc = 0x0D800D800D800D80LL;

  for (int64_t i = 0; i < 0xff; i++)
    {
      int64_t ref = rev_crc64_ref (crc, i);
      int64_t fast = rev_crc64_fast (crc, i);
      if (ref != fast)
	__builtin_abort ();
      crc = ref;
    }

  for (int64_t i = 0; i < 0xff; i++)
    {
      int64_t j = (i << 40) | i;
      int64_t ref = rev_crc64_ref (crc, j);
      int64_t fast = rev_crc64_fast (crc, j);
      if (ref != fast)
	__builtin_abort ();
      crc = ref;
    }

  return 0;
}

/* rev_crc64_fast only: Zbc clmulr (rev_crc64_ref is rv64gc / table).  */
/* { dg-final { scan-assembler-times "clmulr" 1 } } */
