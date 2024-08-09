/* PR target/116287 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtbm -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error \\\(\\\);" "optimized" } } */

#include <x86intrin.h>

extern void link_error (void);

void
tbm_test ()
{
  unsigned int a = 0;
  if (__builtin_ia32_bextri_u32 (a++, 0) != 0)
    link_error ();
  if (__builtin_ia32_bextri_u32 (a++, 0x120) != 0)
    link_error ();
  if (a != 2)
    link_error ();
#ifdef __x86_64__
  unsigned long long b = 0;
  if (__builtin_ia32_bextr_u64 (b++, 0) != 0)
    link_error ();
  if (__builtin_ia32_bextr_u64 (b++, 0x140) != 0)
    link_error ();
  if (b != 2)
    link_error ();
#endif
}
