// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit template specifialization does not
// "inherit" attributes always_inline or noinline from a primary template
// declared with either.  The test disables optimization to verify that
// always_inline forces inlining.
// { dg-do compile }
// { dg-options "-O0 -Wall -fdump-tree-optimized" }

enum Special { };

template <class T>
inline void __attribute__ ((always_inline))
falways_inline_none ()
{
  // Primary template should always be inlined, even without optimization.
  asm ("");   // induce a no-op "side-effect"
}

template <>
inline void
falways_inline_none<Special>()
{
  // The specialization should not be inlined without optimization, even
  // though it's declared inline.
  asm ("");
}

// Verify that a call to the primary is inlined but one to
// the explicit specialization is not.

void test_elim_primary_1 (void)
{
  // Should be inlined.
  falways_inline_none<void>();
// { dg-final { scan-tree-dump-not "falways_inline_none<void> *\\(\\)" "optimized" } }
}

void test_keep_special_1 (void)
{
  // Should not be inlined.
  falways_inline_none<Special>();
// { dg-final { scan-tree-dump-times "falways_inline_none<Special> *\\(\\);" 1 "optimized" } }
}


template <class T>
inline void __attribute__ ((always_inline))
falways_inline_noinline ()
{
  asm ("");   // induce a no-op "side-effect"
}

template <>
void __attribute__ ((noinline))
falways_inline_noinline<Special>() { asm (""); }

// Verify that a call to the primary is inlined but one to
// the explicit specialization is not.

void test_elim_primary_2 (void)
{
  falways_inline_noinline<void>();
// { dg-final { scan-tree-dump-not "falways_inline_noinline<void> *\\(\\)" "optimized" } }
}

void test_keep_special_2 (void)
{
  falways_inline_noinline<Special>();
// { dg-final { scan-tree-dump-times "falways_inline_noinline<Special> *\\(\\);" 1 "optimized" } }
}


template <class T>
inline void
fnone_always_inline ()
{
  asm ("");   // induce a no-op "side-effect"
}

template <>
inline void __attribute__ ((always_inline))
fnone_always_inline<Special>() { asm (""); }

// Verify that a call to the primary is not inlined but one to
// the explicit specialization is.

void test_keep_primary_3 (void)
{
  fnone_always_inline<void>();
// { dg-final { scan-tree-dump-times "fnone_always_inline<void> *\\(\\);" 1 "optimized" } }
}

void test_elim_special_3 (void)
{
  fnone_always_inline<Special>();
// { dg-final { scan-tree-dump-not "fnone_always_inline<Special> *\\(\\);" "optimized" } }
}


template <class T>
void __attribute__ ((noinline))
fnoinline_always_inline ()
{
  asm ("");   // induce a no-op "side-effect"
}

template <>
inline void __attribute__ ((always_inline))
fnoinline_always_inline<Special>()    // { dg-bogus "follows declaration" }
{
  asm ("");
}

// Verify that a call to the primary is not inlined but one to
// the explicit specialization is.

void test_keep_primary_4 (void)
{
  fnoinline_always_inline<void>();
// { dg-final { scan-tree-dump-times "fnoinline_always_inline<void> *\\(\\);" 1 "optimized" } }
}

void test_elim_special_4 (void)
{
  fnoinline_always_inline<Special>();
// { dg-final { scan-tree-dump-not "fnoinline_always_inline<Special> *\\(\\);" "optimized" } }
}
