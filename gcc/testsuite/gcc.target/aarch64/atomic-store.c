/* { dg-do compile } */
/* { dg-options "-march=armv8.4-a -O2" } */

#include <stdatomic.h>

typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;

#define STORE_TESTS(size) \
  void \
  foo##size (int##size##_t *atomic_vals) \
{ \
  atomic_store_explicit (atomic_vals, 2, memory_order_relaxed); \
  atomic_store_explicit (atomic_vals, 2, memory_order_release); \
  atomic_store_explicit ((atomic_vals + 1), 2, memory_order_release); \
  atomic_store ((atomic_vals + 2), 2); \
  atomic_store_explicit ((atomic_vals + 3), 2, memory_order_relaxed); \
}

STORE_TESTS (8);
/* { dg-final { scan-assembler-times "strb\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlrb\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 1  { target { ! ilp32 } } } } */
/* { dg-final { scan-assembler-times "stlrb\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 2  { target { ilp32 } } } } */
/* { dg-final { scan-assembler-times "stlurb\tw\[0-9\]+, \\\[x\[0-9\]+, 1\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlurb\tw\[0-9\]+, \\\[x\[0-9\]+, 2\\\]" 1 } } */
/* { dg-final { scan-assembler-times "strb\tw\[0-9\]+, \\\[x\[0-9\]+, 3\\\]" 1 } } */

STORE_TESTS (16);
/* { dg-final { scan-assembler-times "strh\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlrh\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlurh\tw\[0-9\]+, \\\[x\[0-9\]+, 2\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlurh\tw\[0-9\]+, \\\[x\[0-9\]+, 4\\\]" 1 } } */
/* { dg-final { scan-assembler-times "strh\tw\[0-9\]+, \\\[x\[0-9\]+, 6\\\]" 1 } } */

STORE_TESTS (32);
/* { dg-final { scan-assembler-times "str\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlr\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlur\tw\[0-9\]+, \\\[x\[0-9\]+, 4\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlur\tw\[0-9\]+, \\\[x\[0-9\]+, 8\\\]" 1 } } */
/* { dg-final { scan-assembler-times "str\tw\[0-9\]+, \\\[x\[0-9\]+, 12\\\]" 1 } } */

STORE_TESTS (64);
/* { dg-final { scan-assembler-times "str\tx\[0-9\]+, \\\[x\[0-9\]+\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlur\tx\[0-9\]+, \\\[x\[0-9\]+, 8\\\]" 1 } } */
/* { dg-final { scan-assembler-times "stlur\tx\[0-9\]+, \\\[x\[0-9\]+, 16\\\]" 1 } } */
/* { dg-final { scan-assembler-times "str\tx\[0-9\]+, \\\[x\[0-9\]+, 24\\\]" 1 } } */

void
foo_toolarge_offset (int64_t *atomic_vals)
{
  /* 9bit signed unscaled immediate =>
	largest representable value +255.
	smallest representable value -256.  */
  atomic_store_explicit (atomic_vals + 32, 2, memory_order_release);
  atomic_store_explicit (atomic_vals - 33, 2, memory_order_release);
}

void
foo_negative (int8_t *atomic_vals)
{
  atomic_store_explicit (atomic_vals - 2, 2, memory_order_release);
}
/* { dg-final { scan-assembler-times "stlurb\tw\[0-9\]+, \\\[x\[0-9\]+, -2\\\]" 1 { target { ! ilp32 } } } } */

#pragma GCC target ("arch=armv8.3-a")
void
foo_older_arch (int64_t *atomic_vals)
{
  atomic_store_explicit (atomic_vals + 2, 2, memory_order_release);
}

/* Three times, one for each of the three above functions.  */
/* { dg-final { scan-assembler-times "stlr\tx\[0-9\]+, \\\[x\[0-9\]+\\\]" 4 } } */
