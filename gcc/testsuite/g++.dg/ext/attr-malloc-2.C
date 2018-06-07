// Bug c++/83503 - bogus -Wattributes for const and pure on function template
// specialization
// Test to verify that attribute malloc on multiple declarations of
// the same ordinary function are merged.
// { dg-do compile }
// { dg-options "-O -Wall -fdump-tree-optimized" }

void* __attribute__ ((malloc))
fmalloc_none (unsigned);

void*
fmalloc_none (unsigned);

static char a[8];

void fmalloc_none_failed ();

void test_fmalloc_none (void)
{
  void *p = fmalloc_none (1);
  if (!p)
    return;

  if (p == a)                     // must be false
    fmalloc_none_failed ();       // should be eliminated

  // Verify that the call to fmalloc_none() is eliminated.
  // { dg-final { scan-tree-dump-not "fmalloc_none_failed" "optimized" } }
}

void* fnone_malloc (unsigned);

void* __attribute__ ((malloc))
fnone_malloc (unsigned);

void fnone_malloc_failed ();

void test_fnone_malloc (void)
{
  void *p = fnone_malloc (1);
  if (!p)
    return;

  if (p == a)                     // must be false
    fnone_malloc_failed ();       // should be eliminated

  // Verify that the call to fnone_malloc() is eliminated.
  // { dg-final { scan-tree-dump-not "fnone_malloc_failed" "optimized" } }
}
