/* PR sanitizer/111736 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fsanitize=null,alignment -fdump-tree-optimized -ffat-lto-objects" } */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_type_mismatch" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "p_\[0-9]*.D. \[=!]= 0" "optimized" } } */

#ifdef __x86_64__
#define SEG __seg_fs
#else
#define SEG __seg_gs
#endif

int
foo (int SEG *p, int *q)
{
  return *p;
}

__attribute__((no_sanitize("alignment"))) int
bar (int SEG *p, int *q)
{
  return *p;
}
