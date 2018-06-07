// Bug c++/83503 - bogus -Wattributes for const and pure on function template
// specialization
// Test to verify that an explicit template specifialization does not
// "inherit" attribute malloc from a primary template declared with one.
// { dg-do compile }
// { dg-options "-O -Wall -fdump-tree-optimized" }

template <class>
void* __attribute__ ((malloc))
fmalloc (unsigned);

template <>
void*
fmalloc<int>(unsigned);       // { dg-warning "may be missing attributes" }

static char a[8];

void fmalloc_void_malloc ();
void fmalloc_int_not_malloc ();

void test_fmalloc_primary (void)
{
  void *p = fmalloc<void>(1);
  if (!p)
    return;

  if (p == a)                     // must be false
    fmalloc_void_malloc ();       // should be eliminated

  // Verify that the call to fmalloc_void_malloc() is eliminated.
  // { dg-final { scan-tree-dump-not "fmalloc_void_malloc" "optimized" } }
}


void test_fmalloc_spec_none (void)
{
  void *p = fmalloc<int>(1);
  if (!p)
    return;

  if (p == a)                     // can be true
    fmalloc_int_not_malloc ();    // must not be eliminated

  // Verify that the call to fmalloc_int_not_malloc() is retained.
  // { dg-final { scan-tree-dump-times "fmalloc_int_not_malloc" 1 "optimized" } }
}

template <>
void*
fmalloc<long>(unsigned);          // { dg-warning "may be missing attributes" }

template <>
void* __attribute__ ((malloc))
fmalloc<long>(unsigned);

void fmalloc_long_malloc ();

void test_fmalloc_spec_malloc (void)
{
  void *p = fmalloc<long>(1);
  if (!p)
    return;

  if (p == a)                     // can be true
    fmalloc_long_malloc ();       // must not be eliminated

  // Verify that the call to fmalloc_long_malloc() is eliminated.
  // { dg-final { scan-tree-dump-not "fmalloc_long_malloc" "optimized" } }
}
