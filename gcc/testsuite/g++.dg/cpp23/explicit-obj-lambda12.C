// P0847R7
// { dg-do compile { target c++23 } }

// SFINAE when the call operator for a lambda with captures is instantiated
// with an unrelated type.
// diagnose ambiguous overloads when the call operator for a captureless lambda is instantiated
// with an unrelated type.

// overload resolution when taking address of function

/* [expr.prim.lambda.general-5]

   Given a lambda with a lambda-capture, the type of the explicit object
   parameter, if any, of the lambda's function call operator (possibly
   instantiated from a function call operator template) shall be either:

   --(5.1) the closure type,
   --(5.2) a class type derived from the closure type, or
   --(5.3) a reference to a possibly cv-qualified such type.  */

// The above wording is similar to [dcl.fct-15] which is handled by SFINAE,
// thus we also handle the following cases the same way.

// We need the 2 overloads to be ambiguous to observe substitution failure
// for the lambda's call operator when instantiated with an unrelated type.
// We accomplish this by introducing both overloads through using declarations.

struct B0 {
  void operator()(this auto) {}
};
template<typename T>
struct S0 : T, B0 {
  using B0::operator();
  using T::operator();
};

void test0()
{
  auto s0 = S0{[](this auto){}};
  void (*p0)(int) = &decltype(s0)::operator(); // { dg-error {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} }

  auto s1 = S0{[x = 42](this auto){}};
  void (*p1)(int) = &decltype(s1)::operator(); // { dg-bogus {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} {Substitution failure for a captureful lambda with an unrelated xobj parameter type failed!} }
}

struct B1 {
  void operator()(this auto&&) {}
};
template<typename T>
struct S1 : T, B1 {
  using B1::operator();
  using T::operator();
};

void test1()
{
  auto s0 = S1{[](this auto&&){}};
  void (*p0)(int&) = &decltype(s0)::operator(); // { dg-error {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} }

  auto s1 = S1{[x = 42](this auto&&){}};
  void (*p1)(int&) = &decltype(s1)::operator(); // { dg-bogus {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} {Substitution failure for a captureful lambda with an unrelated xobj parameter type failed!} }
}


struct B2 {
  // not a template, should be taken over the lambda's call operator
  void operator()(this int&) {}
};
template<typename T>
struct S2 : T, B2 {
  using T::operator();
  using B2::operator();
};

void test2()
{
  auto s0 = S2{[](this auto&&){}};
  void (*p0)(int&) = &decltype(s0)::operator(); // { dg-bogus {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} }

  auto s1 = S2{[x = 42](this auto&&){}};
  void (*p1)(int&) = &decltype(s1)::operator(); // { dg-bogus {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} }
}

struct B3 {
  // must be a template so it is not taken over the lambda's call operator
  template<typename U = void>
  void operator()(this int&) {}
};
template<typename T>
struct S3 : T, B3 {
  using B3::operator();
  using T::operator();
};

void test3()
{
  auto s0 = S3{[](this auto&&){}};
  void (*p0)(int&) = &decltype(s0)::operator(); // { dg-error {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} }

  auto s1 = S3{[x = 42](this auto&&){}};
  void (*p1)(int&) = &decltype(s1)::operator(); // { dg-bogus {converting overloaded function '[^\n\r]+' to type '[^\n\r]+' is ambiguous} {Substitution failure for a captureful lambda with an unrelated xobj parameter type failed!} }
}

