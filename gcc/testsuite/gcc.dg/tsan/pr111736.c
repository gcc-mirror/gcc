/* PR sanitizer/111736 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fsanitize=thread -fdump-tree-optimized -ffat-lto-objects" } */
/* { dg-final { scan-tree-dump-not "__tsan_read" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__tsan_write" "optimized" } } */

#ifdef __x86_64__
#define SEG __seg_fs
#else
#define SEG __seg_gs
#endif

void
foo (int SEG *p, int SEG *q)
{
  *q = *p;
}
