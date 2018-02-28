// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit template specifialization does not
// "inherit" attribute nothrow from a primary template declared with one.
// { dg-do compile }
// { dg-options "-O -Wall -fdump-tree-optimized" }

template <class T>
void __attribute__ ((nothrow))
f ();

template <>
void f<int>();

void f_void_nothrow ();
void f_int_maythrow ();

void fv (void)
{
  try
    {
      f<void>();
    }
  catch (...)                    // cannot be be reached
    {
      f_void_nothrow ();         // should be eliminated
    }
}


void fi (void)
{
  try
    {
      f<int>();
    }
  catch (...)                    // may be reached
    {
      f_int_maythrow ();         // must not be eliminated
    }
}

// Verify that the call to f_void_nothrow() is eliminated but
// the call to f_int_maythrow() is retained.
// { dg-final { scan-tree-dump-not "f_void_nothrow" "optimized" } }
// { dg-final { scan-tree-dump-times "f_int_maythrow" 1 "optimized" } }
