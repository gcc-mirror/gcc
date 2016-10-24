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
  if (_pext_u32 (0xabcdef98, 0xffff0000) != 0xabcd
      || _pext_u32 (0xabcdef98, 0xffffff00) != 0xabcdef
      || _pext_u32 (0xabcdef98, 0x0f0f0f0f) != 0xbdf8
      || _pext_u32 (0xabcdef98, 0xff0fff0f) != 0xabdef8
      || _pext_u32 (0xabcdef98, 0x000fffff) != 0xdef98
      || _pext_u32 (a, 0xffffffff) != a)
    link_error ();
#ifdef __x86_64__
  if (_pext_u64 (0xabcdef9876543210UL, 0xffffffff00000000UL) != 0xabcdef98UL
      || _pext_u64 (0xabcdef9876543210UL, 0xffffffffffffff00UL) != 0xabcdef98765432UL
      || _pext_u64 (0xabcdef9876543210UL, 0x0f0f0f0f0f0f0f0fUL) != 0xbdf86420UL
      || _pext_u64 (0xabcdef9876543210UL, 0xff0fff0fff0fff0fUL) != 0xabdef8764320UL
      || _pext_u64 (0xabcdef9876543210UL, 0x00000000000fffffUL) != 0x43210UL
      || _pext_u64 (b, 0xffffffffffffffffUL) != b)
    link_error ();
#endif
  return 0;
}
