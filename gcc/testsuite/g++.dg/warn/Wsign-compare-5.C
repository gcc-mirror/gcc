// Test that -Wsign-compare doesn't warn about
// equality/non-equality comparisons with sizeof.
// { dg-do compile }
// { dg-options "-Wsign-compare" }

int
foo (int x)
{
  if (x != sizeof (sizeof (x)))		// { dg-bogus "comparison between signed and unsigned integer expressions" }
    return 1;
  return 0;
}

int
bar (int x)
{
  if (x == sizeof (sizeof (x)) + 1)	// { dg-bogus "comparison between signed and unsigned integer expressions" }
    return 1;
  return 0;
}
