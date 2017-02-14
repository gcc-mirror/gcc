/* { dg-do compile } */
/* { dg-options "-O2 -mbmi2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error" "optimized" } } */

#include <x86intrin.h>

extern void link_error (void);

unsigned int a;
unsigned long long b;

int
main ()
{
  asm volatile ("" : : "g" (&a), "g" (&b) : "memory");
  if (_pdep_u32 (0xabcdef98, 0xffff0000) != 0xef980000
      || _pdep_u32 (0xabcdef98, 0xffffff00) != 0xcdef9800
      || _pdep_u32 (0xabcdef98, 0x0f0f0f0f) != 0x0e0f0908
      || _pdep_u32 (0xabcdef98, 0xff0fff0f) != 0xcd0ef908
      || _pdep_u32 (0xabcdef98, 0x000fffff) != 0xdef98
      || _pdep_u32 (a, 0xffffffff) != a)
    link_error ();
#ifdef __x86_64__
  if (_pdep_u64 (0xabcdef9876543210UL, 0xffffffff00000000UL) != 0x7654321000000000UL
      || _pdep_u64 (0xabcdef9876543210UL, 0xffffffffffffff00UL) != 0xcdef987654321000UL
      || _pdep_u64 (0xabcdef9876543210UL, 0x0f0f0f0f0f0f0f0fUL) != 0x0706050403020100UL
      || _pdep_u64 (0xabcdef9876543210UL, 0xff0fff0fff0fff0fUL) != 0xef09870654032100UL
      || _pdep_u64 (0xabcdef9876543210UL, 0x00000000000fffffUL) != 0x43210UL
      || _pdep_u64 (b, 0xffffffffffffffffUL) != b)
    link_error ();
#endif
  return 0;
}
