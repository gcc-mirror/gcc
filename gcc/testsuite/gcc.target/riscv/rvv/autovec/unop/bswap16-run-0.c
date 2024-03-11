/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model" } */

#include <stdint-gcc.h>
#include "test-math.h"

#define ARRAY_SIZE 128

uint16_t in[ARRAY_SIZE];
uint16_t out[ARRAY_SIZE];
uint16_t ref[ARRAY_SIZE];

TEST_UNARY_CALL (uint16_t, __builtin_bswap16)
TEST_ASSERT (uint16_t)

/* TEST_INIT Arguments:
	  +-------+-------+---------------------------+---------+
	  | type  | input | reference                 | test id |
	  +-------+-------+---------------------------+---------+
*/
TEST_INIT (uint16_t, 0x1234u, __builtin_bswap16 (0x1234u), 1)
TEST_INIT (uint16_t, 0x1122u, __builtin_bswap16 (0x1122u), 2)
TEST_INIT (uint16_t, 0xa55au, __builtin_bswap16 (0xa55au), 3)
TEST_INIT (uint16_t, 0x0000u, __builtin_bswap16 (0x0000u), 4)
TEST_INIT (uint16_t, 0xffffu, __builtin_bswap16 (0xffffu), 5)
TEST_INIT (uint16_t, 0x4321u, __builtin_bswap16 (0x4321u), 6)

int
main ()
{
  /* RUN_TEST Arguments:
	   +------+---------+-------------+----+-----+-----+------------+
	   | type | test id | fun to test | in | out | ref | array size |
	   +------+---------+-------------+----+-----+-----+------------+
  */
  RUN_TEST (uint16_t, 1, __builtin_bswap16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (uint16_t, 2, __builtin_bswap16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (uint16_t, 3, __builtin_bswap16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (uint16_t, 4, __builtin_bswap16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (uint16_t, 5, __builtin_bswap16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (uint16_t, 6, __builtin_bswap16, in, out, ref, ARRAY_SIZE);

  return 0;
}
