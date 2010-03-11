/* { dg-do run } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-O2" } */

#include <stdint.h>

extern void abort (void);

uint64_t __attribute__((noinline))
byteswap64(uint64_t x)
{
  uint32_t a = x >> 32;
  uint32_t b = (uint32_t) x;
  return ((uint64_t) ((((((b)) >> (8)) | (((b)) << (32 - (8)))) & 0xff00ff00L)
		      | (((((b)) << (8)) | (((b)) >> (32 - (8)))) & 0x00ff00ffL)) << 32)
          | (uint64_t) ((((((a)) >> (8)) | (((a)) << (32 - (8)))) & 0xff00ff00L)
			| (((((a)) << (8)) | (((a)) >> (32 - (8)))) & 0x00ff00ffL));
}

int
main ()
{
  uint64_t in = (uint64_t)0x01020304 << 32 | 0x05060708;
  uint64_t cmp = (uint64_t)0x08070605 << 32 | 0x04030201;

  if (cmp != byteswap64 (in))
    abort ();

  return 0;
}
