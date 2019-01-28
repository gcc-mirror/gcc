// PR c++/37967
// Negative test for auto
// { dg-do compile { target c++11 } }

auto f1 () -> int;
auto f2 ();		// { dg-error "1:.f2. function uses .auto. type specifier without trailing return type" "" { target { ! c++14 } } }
int f3 () -> int;	// { dg-error "1:.f3. function with trailing return type" }
auto *f4 () -> int;	// { dg-error "1:.f4. function with trailing return type" }

struct A
{
  auto f5 () const -> int;
  auto f6 ();		// { dg-error "3:.f6. function uses .auto. type specifier without trailing return type" "" { target { ! c++14 } } }
  int f7 () -> int;	// { dg-error "3:.f7. function with trailing return type" }
  auto *f8 () -> int;	// { dg-error "3:.f8. function with trailing return type" }
};
