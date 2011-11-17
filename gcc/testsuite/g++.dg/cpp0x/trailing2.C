// PR c++/37967
// Negative test for auto
// { dg-options "-std=c++0x" }

auto f1 () -> int;
auto f2 ();		// { dg-error "without trailing return type" }
int f3 () -> int;	// { dg-error "trailing return type" }
auto *f4 () -> int;	// { dg-error "trailing return type" }

struct A
{
  auto f5 () const -> int;
  auto f6 ();		// { dg-error "without trailing return type" }
  int f7 () -> int;	// { dg-error "trailing return type" }
  auto *f8 () -> int;	// { dg-error "trailing return type" }
};
