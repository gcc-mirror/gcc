// P0847R7
// { dg-do compile { target c++23 } }

// SFINAE when the call operator for a lambda with captures is instantiated
// with an unrelated type.
// diagnose ambiguous overloads when the call operator for a captureless lambda is instantiated
// with an unrelated type.

// overload resolution from call expression

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
  using T::operator();
  using B0::operator();
  operator int() const {return {};}
};

void test0()
{
  auto s0 = S0{[](this auto){}};
  s0.operator()<int>(); // { dg-error {call of overloaded 'operator\(\)\(\)' is ambiguous} }

  auto s1 = S0{[x = 42](this auto){}};
  s1.operator()<int>(); // { dg-bogus {call of overloaded 'operator\(\)\(\)' is ambiguous} }
}


struct B1 {
  void operator()(this auto&&) {}
};
template<typename T>
struct S1 : T, B1 {
  using T::operator();
  using B1::operator();
  operator int() const {return {};}
};

void test1()
{
  auto s0 = S1{[](this auto&&){}};
  s0.operator()<int>(); // { dg-error {call of overloaded 'operator\(\)\(\)' is ambiguous} }

  auto s1 = S1{[x = 42](this auto&&){}};
  s1.operator()<int>(); // { dg-bogus {call of overloaded 'operator\(\)\(\)' is ambiguous} }
}


struct B2 {
  // needs to be a template, we are explicitly passing a template argument,
  // without the parameter here this would not be a candidate
  template<typename U = void>
  void operator()(this int) {}
};

template<typename T>
struct S2 : T, B2 {
  using T::operator();
  using B2::operator();
  operator int() const {return {};}
};

// I don't know why the calls to s0::operator() are not ambiguous, it might have to do with one taking less conversions, I'm not sure.
// Someone who knows better should remove those cases if they are sure they are actually correct.

void test2()
{
  auto s0 = S2{[](this auto){}};
  s0.operator()<int>(); // { dg-error {call of overloaded 'operator\(\)\(\)' is ambiguous} {Not sure if this is a bug, one might be a better conversion} { xfail *-*-* } }

  auto s1 = S2{[x = 42](this auto){}};
  s1.operator()<int>(); // { dg-bogus {call of overloaded 'operator\(\)\(\)' is ambiguous} }
}

void test3()
{
  auto s0 = S2{[](this auto&&){}};
  s0.operator()<int>(); // { dg-error {call of overloaded 'operator\(\)\(\)' is ambiguous} {Not sure if this is a bug, one might be a better conversion} { xfail *-*-* } }

  auto s1 = S2{[x = 42](this auto&&){}};
  s1.operator()<int>(); // { dg-bogus {call of overloaded 'operator\(\)\(\)' is ambiguous} }
}

