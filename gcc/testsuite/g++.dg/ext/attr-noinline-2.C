// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit template specifialization does not
// "inherit" attributes always_inline or noinline from a primary template
// declared with either.  Unlike attr-noinline.C, this test enables
// optimization to verify that noinline prevents inlining.
// { dg-do compile }
// { dg-options "-O2 -Wall -fdump-tree-optimized" }

enum Special { };

int global;

template <class T>
inline void __attribute__ ((always_inline))
falways_inline_noinline ()
{
  // Create a side-effect that's unique to this function.
  global = __LINE__;
}

template <>
void __attribute__ ((noinline))
falways_inline_noinline<Special>()
{
  global = __LINE__;
}

// Verify that a call to the primary is inlined but one to
// the explicit specialization is not.

void test_elim_primary_1 (void)
{
  // Should be inlined.
  falways_inline_noinline<void>();
// { dg-final { scan-tree-dump-not "falways_inline_noinline<void> *\\(\\)" "optimized" } }
}

void test_keep_special_1 (void)
{
  // Should not be inlined.
  falways_inline_noinline<Special>();
// { dg-final { scan-tree-dump-times "falways_inline_noinline<Special> *\\(\\);" 1 "optimized" } }
}


template <class T>
void __attribute__ ((noinline))
fnoinline_always_inline ()
{
  global = __LINE__;
}

template <>
inline void __attribute__ ((always_inline))
fnoinline_always_inline<Special>()    // { dg-bogus "follows declaration" }
{
  global = __LINE__;
}

void test_keep_primary_2 (void)
{
  // Should not be inlined.
  fnoinline_always_inline<void>();
// { dg-final { scan-tree-dump-times "fnoinline_always_inline<void> *\\(\\);" 1 "optimized" } }
}

void test_elim_special_2 (void)
{
  // Should be inlined.
  fnoinline_always_inline<Special>();
// { dg-final { scan-tree-dump-not "fnoinline_always_inline<Special> *\\(\\);" "optimized" } }
}
