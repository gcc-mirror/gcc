// P2564R3
// { dg-do compile { target c++20 } }
// From cxx2b-consteval-propagate.cpp.

void side_effect();

consteval int
f (int x)
{
  if (!x)
    side_effect(); // { dg-error "call to non-.constexpr. function" }
  return x;
}

struct SS {
  int y = f(1);
  int x = f(0);
  SS();
};
SS::SS(){} // { dg-error "call to consteval function" }

consteval int
f2 (int x)
{
  if (!__builtin_is_constant_evaluated ())
    side_effect();
  return x;
}

struct S2 {
  int x = f2(0);
  constexpr S2();
};

constexpr S2::S2(){}
S2 s = {};
constinit S2 s2 = {};

struct S3 {
  int x = f2(0);
  S3();
};
S3::S3(){}

consteval int undef (int x); // { dg-warning "never defined" }

struct X {
  int a = sizeof(undef(0));
  int x = undef(0);

  X() = default; // { dg-error {'consteval int undef\(int\)' used before its definition} }
};

void
test ()
{
  [[maybe_unused]] X x; // { dg-error "call to consteval function" }
// { dg-message "promoted to an immediate function" "" { target *-*-* } .-1 }
}
