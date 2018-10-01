// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit function template specifialization
// does not "inherit" attribute nonnull from an argument declared with
// one in the primary template.
// { dg-do compile }
// { dg-options "-O -Wall -fdump-tree-optimized -fdelete-null-pointer-checks" }

template <class T>
void* __attribute__ ((returns_nonnull))
g ();

template <>
void*
g<int>();

extern void g_void_returns_nonnull ();
extern void g_int_may_return_null ();

void test_returns_nonnull ()
{
  void *p = g<void>();
  if (!p)
    g_void_returns_nonnull ();

  (void)&p;
}

void test_may_return_null ()
{
  void *p = g<int>();
  if (!p)
    g_int_may_return_null ();

  (void)&p;
}


// Verify that the call to g_void_returns_nonnull() is eliminated but
// the call to g_int_may_return_null() is retained.
// { dg-final { scan-tree-dump-not "g_void_returns_nonnull" "optimized" } }
// { dg-final { scan-tree-dump-times "g_int_may_return_null" 1 "optimized" } }
