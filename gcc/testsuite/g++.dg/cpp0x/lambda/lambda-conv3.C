// Conversion to a function pointer uses a generic thunk, which doesn't
// work properly for variadics.  Make sure that we can still use the lambda
// normally.

// { dg-options -std=c++0x }

void f()
{
  auto l = [](...){};
  void (*p1)(...) = l;		// { dg-bogus "sorry" "" { xfail *-*-* } }
  l();				// OK
}
