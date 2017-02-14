/* { dg-do compile } */
/* { dg-options "-O2 -mtbm -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */

#include <x86intrin.h>

extern void link_error (void);

volatile unsigned int a;
volatile unsigned long long b;

int
main ()
{
  if (__bextri_u32 (0xffffffffU, 0 | (0 << 8)) != 0
      || __bextri_u32 (0xffffffffU, 64 | (16 << 8)) != 0
      || __bextri_u32 (0x12345678U, 4 | (10 << 8)) != 0x167
      || __bextri_u32 (0xffffffffU, 2 | (255 << 8)) != 0x3fffffff
      || __bextri_u32 (0xdeadbeefU, 2 | (64 << 8)) != 0x37ab6fbb
      || __bextri_u32 (0xdeadbeefU, 0 | (64 << 8)) != 0xdeadbeefU
      || __bextri_u32 (a, 0 | (0 << 8)) != 0
      || __bextri_u32 (a, 32 | (16 << 8)) != 0)
    link_error ();
#ifdef __x86_64__
  if (__bextri_u64 (0xffffffffffffffffUL, 0 | (0 << 8)) != 0
      || __bextri_u64 (0xffffffffffffffffUL, 128 | (16 << 8)) != 0
      || __bextri_u64 (0x123456789abcdef0UL, 5 | (37 << 8)) != 0x13c4d5e6f7UL
      || __bextri_u64 (0xffffffffffffffffUL, 2 | (255 << 8)) != 0x3fffffffffffffffUL
      || __bextri_u64 (0xdeadbeefbeefdeadU, 2 | (64 << 8)) != 0x37ab6fbbefbbf7abUL
      || __bextri_u64 (0xdeadbeefbeefdeadU, 0 | (64 << 8)) != 0xdeadbeefbeefdeadUL
      || __bextri_u64 (b, 0 | (0 << 8)) != 0
      || __bextri_u64 (b, 64 | (16 << 8)) != 0)
    link_error ();
#endif
  return 0;
}
