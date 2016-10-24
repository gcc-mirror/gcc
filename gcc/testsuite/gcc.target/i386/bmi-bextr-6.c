/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */

#include <x86intrin.h>

extern void link_error (void);

volatile unsigned int a;
volatile unsigned long long b;

int
main ()
{
  if (__bextr_u32 (0xffffffffU, 0 | (0 << 8)) != 0
      || __bextr_u32 (0xffffffffU, 64 | (16 << 8)) != 0
      || __bextr_u32 (0x12345678U, 4 | (10 << 8)) != 0x167
      || __bextr_u32 (0xffffffffU, 2 | (255 << 8)) != 0x3fffffff
      || __bextr_u32 (0xdeadbeefU, 2 | (64 << 8)) != 0x37ab6fbb
      || __bextr_u32 (0xdeadbeefU, 0 | (64 << 8)) != 0xdeadbeefU
      || __bextr_u32 (a, 0 | (0 << 8)) != 0
      || __bextr_u32 (a, 32 | (16 << 8)) != 0)
    link_error ();
  if (_bextr_u32 (0xffffffffU, 0, 0) != 0
      || _bextr_u32 (0xffffffffU, 64, 16) != 0
      || _bextr_u32 (0x12345678U, 4, 10) != 0x167
      || _bextr_u32 (0xffffffffU, 2, 255) != 0x3fffffff
      || _bextr_u32 (0xdeadbeefU, 2, 64) != 0x37ab6fbb
      || _bextr_u32 (0xdeadbeefU, 0, 64) != 0xdeadbeefU
      || _bextr_u32 (a, 0, 0) != 0
      || _bextr_u32 (a, 32, 16) != 0)
    link_error ();
#ifdef __x86_64__
  if (__bextr_u64 (0xffffffffffffffffUL, 0 | (0 << 8)) != 0
      || __bextr_u64 (0xffffffffffffffffUL, 128 | (16 << 8)) != 0
      || __bextr_u64 (0x123456789abcdef0UL, 5 | (37 << 8)) != 0x13c4d5e6f7UL
      || __bextr_u64 (0xffffffffffffffffUL, 2 | (255 << 8)) != 0x3fffffffffffffffUL
      || __bextr_u64 (0xdeadbeefbeefdeadU, 2 | (64 << 8)) != 0x37ab6fbbefbbf7abUL
      || __bextr_u64 (0xdeadbeefbeefdeadU, 0 | (64 << 8)) != 0xdeadbeefbeefdeadUL
      || __bextr_u64 (b, 0 | (0 << 8)) != 0
      || __bextr_u64 (b, 64 | (16 << 8)) != 0)
    link_error ();
  if (_bextr_u64 (0xffffffffffffffffUL, 0, 0) != 0
      || _bextr_u64 (0xffffffffffffffffUL, 128, 16) != 0
      || _bextr_u64 (0x123456789abcdef0UL, 5, 37) != 0x13c4d5e6f7UL
      || _bextr_u64 (0xffffffffffffffffUL, 2, 255) != 0x3fffffffffffffffUL
      || _bextr_u64 (0xdeadbeefbeefdeadUL, 2, 64) != 0x37ab6fbbefbbf7abUL
      || _bextr_u64 (0xdeadbeefbeefdeadUL, 0, 64) != 0xdeadbeefbeefdeadUL
      || _bextr_u64 (b, 0, 0) != 0
      || _bextr_u64 (b, 64, 16) != 0)
    link_error ();
#endif
  return 0;
}
