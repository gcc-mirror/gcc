// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on id-expression.

template<typename T>
void foo (T) { }

template<typename T>
void foo (T, T) { }

void
f (int p)
{
  constexpr auto r1 = ^^foo<int>;   // { dg-error "insufficient contextual information" }
  constexpr auto r2 = ^^::foo<int>; // { dg-error "insufficient contextual information" }

  constexpr auto r3 = ^^__func__;   // { dg-error "cannot be applied" }
  constexpr auto r4 = ^^__FUNCTION__; // { dg-error "cannot be applied" }
  constexpr auto r5 = ^^__PRETTY_FUNCTION__;  // { dg-error "cannot be applied" }

  requires(int a) { ^^p; };
  requires(int a) { ^^a; }; // { dg-error "cannot be applied to a local parameter of a requires-expression .a." }
}

// NTTPs and pack-index-expressions cannot appear as operands
// of the reflection operator.

struct S { int i; };
enum E { EE };
static constexpr int glob = 0;

template<S s, int n, E e, double d, const int& r>
void
g ()
{
  constexpr auto r1 = ^^s;  // { dg-error "cannot be applied to a non-type template parameter .s." }
  constexpr auto r2 = ^^n;  // { dg-error "cannot be applied to a non-type template parameter .n." }
  constexpr auto r3 = ^^e;  // { dg-error "cannot be applied to a non-type template parameter .e." }
  constexpr auto r4 = ^^d;  // { dg-error "cannot be applied to a non-type template parameter .d." }
  constexpr auto r5 = ^^r;  // { dg-error "cannot be applied to a non-type template parameter .r." }
}

template<typename T, T t>
void
g2 ()
{
  constexpr auto r = ^^t; // { dg-error "cannot be applied to a non-type template parameter .t." }
}

void
h ()
{
  constexpr S s{};
  constexpr int n = 42;
  constexpr E e{};
  constexpr double d = 0.0;
  g<s, n, e, d, glob>();

  g2<int, 42>();
}
