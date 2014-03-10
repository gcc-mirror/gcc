// Conversion to a function pointer uses a generic thunk, which doesn't
// work properly for variadics.  Make sure that we can still use the lambda
// normally.

// { dg-do compile { target c++11 } }

void f()
{
  auto l = [](...){};
  void (*p1)(...) = l;		// { dg-bogus "sorry" "" { xfail *-*-* } }
  l();				// OK
}
