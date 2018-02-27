// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit template specifialization does not
// "inherit" attribute optimize from a primary template declared with
// one.
// { dg-do compile }
// { dg-options "-O2 -Wall -fdump-tree-optimized" }

enum Special { };

void foptimize_none_primary_failed ();

template <class T>
void __attribute__ ((optimize ("no-printf-return-value")))
foptimize_none ()
{
  // The call to snprintf and the test should be retained.
  if (2 != __builtin_snprintf (0, 0, "%hhx", 0x12))
    foptimize_none_primary_failed ();
}

void foptimize_none_special_failed ();

template <>
inline void
foptimize_none<Special>()
{
  // The whole if statement should be eliminated.
  if (3 != __builtin_snprintf (0, 0, "1%hhx", 0x12))
    foptimize_none_special_failed ();
}

void test_primary ()
{
  foptimize_none<void>();
  // { dg-final { scan-tree-dump-times "foptimize_none_primary_failed *\\(\\)" 1 "optimized" } }
}

void test_special ()
{
  // Should be eliminated.
  foptimize_none<Special>();
// { dg-final { scan-tree-dump-not "foptimize_none_special_failed *\\(\\)" "optimized" } }
}

// { dg-final { scan-tree-dump-times "__builtin_snprintf" 1 "optimized" } }
