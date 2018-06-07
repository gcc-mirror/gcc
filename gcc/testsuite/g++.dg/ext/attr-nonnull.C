// Bug c++/83871 - wrong code due to attributes on distinct template
// specializations
// Test to verify that an explicit function template specifialization
// does not "inherit" attribute nonnull from an argument declared with
// one in the primary template.
// { dg-do compile }
// { dg-options "-O -Wall" }

template <class T>
void __attribute__ ((nonnull (1)))
f (T*, T*, T*);

template <>
void
f<int>(int*, int*, int*);     // { dg-warning "may be missing attributes" }

template <>
void __attribute__ ((nonnull (3)))
f<float>(float*, float*, float*);


void test_nonnull (void)
{
  f<void>(0, 0, 0);           // { dg-warning "null argument where non-null required \\\(argument 1\\\)" }

  f<int>(0, 0, 0);            // { dg-bogus "null argument" }

  f<float>(0, 0, 0);
  // { dg-bogus "null argument where non-null required \\\(argument 1\\\)" "" { target *-*-* } .-1 }
  // { dg-warning "null argument where non-null required \\\(argument 3\\\)" "" { target *-*-* } .-2 }
}
