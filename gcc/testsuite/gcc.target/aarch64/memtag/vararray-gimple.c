/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-asan -O2" } */

extern int *use (int *b, int n);

int *foo (int n)
{
  int b[n];
  return use (b, n);
}

/* HWASAN_ALLOCA_POISON is used for alloca and VLAs when MEMTAG is in effect.
   Although HWASAN_ALLOCA_UNPOISON is (also) used for untagging frame, it
   doesnt hurt to check it in context of the current test.  */
/* { dg-final { scan-tree-dump "HWASAN_ALLOCA_POISON" "asan1" } } */
/* { dg-final { scan-tree-dump "HWASAN_ALLOCA_UNPOISON" "asan1" } } */
