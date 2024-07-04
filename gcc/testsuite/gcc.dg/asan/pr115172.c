/* PR sanitizer/115172 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fsanitize=address,bool -ffat-lto-objects -fdump-tree-asan1" } */
/* { dg-final { scan-tree-dump-not "\.ASAN_CHECK " "asan1" } } */

#ifdef __x86_64__
#define SEG __seg_gs
#else
#define SEG __seg_fs
#endif

extern struct S { _Bool b; } s;
void bar (void);

void
foo (void)
{
  if (*(volatile _Bool SEG *) (__UINTPTR_TYPE__) &s.b)
    bar ();
}
