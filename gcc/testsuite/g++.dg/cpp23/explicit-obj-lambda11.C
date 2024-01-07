// P0847R7
// { dg-do compile { target c++23 } }

// unrelated xobj parameter type in captureless lambdas and lambdas with captures

struct S0{};

void test0()
{
  auto f0 = [](this S0){ return 5; }; // { dg-bogus "a lambda with captures may not have an explicit object parameter of an unrelated type" }
  auto f1 = [x = 42](this S0){ return 5; }; // { dg-error "a lambda with captures may not have an explicit object parameter of an unrelated type" }
}

// instantiation by calling with explicit template arguments

template<typename T>
struct S1 : T {
  using T::operator();
  operator int() const {return {};}
};

void test1()
{
  auto s0 = S1{[](this auto&& self) { return self; }}; // { dg-bogus {a lambda with captures may not have an explicit object parameter of an unrelated type} }
  s0.operator()<int>(); // { dg-bogus {no matching function for call to} }

  auto s1 = S1{[x = 0](this auto&& self) { return self; }}; // { dg-line t1_s1 }
  s1.operator()<int>(); // { dg-error {no matching function for call to} }
}
// { dg-note {candidate:} {} { target *-*-* } t1_s1 }
// { dg-note {template argument deduction/substitution failed} {} { target *-*-* } t1_s1 }
// { dg-error {a lambda with captures may not have an explicit object parameter of an unrelated type} {} { target *-*-* } t1_s1 }

// instantiation from overload resolution when taking address of call operator

void test2()
{
  auto f = [x = 42](this auto&&){ return x; }; // { dg-line t2_f }

  int (*fp0)(decltype(f)&) = &decltype(f)::operator();
  int (*fp1)(int&) = &decltype(f)::operator(); // { dg-error {no matches converting function} }
}

// { dg-error "a lambda with captures may not have an explicit object parameter of an unrelated type" {depends on PR112874} { xfail *-*-* } t2_f }
// { dg-note "candidate is" "" { target *-*-* } t2_f }

