// PR c++/69023 - bitset whose name is used in constant-expression rejected
// Test also verifies the correct evaluation of the expressions with
// -fpermissive.
// { dg-options "-fpermissive" }

#if __cplusplus >= 201103L
#  define ASSERT(e) static_assert (e, #e)
#else
#  define ASSERT(e)                                             \
  do { struct S { bool: !!(e); } asrt; (void)&asrt; } while (0)
#endif


void test_bitset ()
{
  int x;                        // { dg-warning "changes meaning" }

  {
    struct S {
      int x: sizeof x;          // { dg-warning "declaration" }
    };
  }
}

void test_enum ()
{
  // Also exercise (not covered by c++/69023):
  int y;                        // { dg-warning "changes meaning" }
  {
    struct S {
      enum E {
        y = sizeof y            // { dg-warning "declaration" }
      };

      // Verify the enumerator has the correct value.
      void test () { ASSERT (y == sizeof (int)); }
    };
  }
}

void test_alignas ()
{
  enum { A = 16 };              // { dg-warning "changes meaning" }
  {
    struct S {
#if __cplusplus >= 201103L
      alignas (A)
#else
      __attribute__ ((aligned (A)))
#endif
      int A;                    // { dg-warning "declaration" }

      // Verify the member has the correct alignment.
      void test () { ASSERT (__alignof__ (this->A) == 16); }
    };
  }
}

void test_array ()
{
  enum { A = 16 };              // { dg-warning "changes meaning" }
  {
    struct S {
      int A [A];                // { dg-warning "declaration" }

      // Verify the member has the correct alignment.
      void test () { ASSERT (sizeof (this->A) == 16 * sizeof (int)); }
    };
  } 
}

void test_vector ()
{
  enum { A = 16 };              // { dg-warning "changes meaning" }
  {
    struct S {
      int A __attribute__ ((vector_size (A))); // { dg-warning "declaration" }

      // Verify the member has the correct size.
      void test () { ASSERT (sizeof (this->A) == 16); }
    };
  } 
}
 
