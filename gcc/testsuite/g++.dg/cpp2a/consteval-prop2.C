// P2564R3
// { dg-do compile { target c++20 } }
// Testcase from P2564R3.

consteval int id(int i) { return i; }
constexpr char id(char c) { return c; }

template<class T>
constexpr int f(T t) {
  return t + id(t);		// { dg-message "immediate-escalating expression .id\\(t\\)." }
}

auto a = &f<char>;              // OK, f<char> is not an immediate function
auto b = &f<int>;               // { dg-error "taking address of an immediate function" }

static_assert(f(3) == 6);       // OK

template<class T>
constexpr int g(T t) {          // g<int> is not an immediate function
  return t + id(42);            // because id(42) is already a constant
}

template<class T, class F>
constexpr bool is_not(T t, F f) {
  return not f(t);
}

consteval bool is_even(int i) { return i % 2 == 0; }

static_assert(is_not(5, is_even));      // OK

int x = 0;

template<class T>
constexpr T h(T t = id(x)) {    // h<int> is not an immediate function
    return t;
}

template<class T>
constexpr T hh() {              // hh<int> is an immediate function
  return h<T>();		// { dg-error "the value of .x. is not usable in a constant expression" }
// { dg-message "immediate-escalating expression .id\\(x\\)." "" { target *-*-* } .-1 }
}

int i = hh<int>();              // { dg-error "call to consteval function|called in a constant expression" }
				// error: hh<int>() is an immediate-escalating expression
                                // outside of an immediate-escalating function
struct A {
  int x;
  int y = id(x);
};

// [expr.const]#example-9 says:
//   k<int> is not an immediate function because A(42) is a
//   constant expression and thus not immediate-escalating
// In the evaluation of A(42), the member x has just been initialized
// to constant 42.  And A(42) is constant-evaluated because "An aggregate
// initialization is an immediate invocation if it evaluates a default
// member initializer that has a subexpression that is an
// immediate-escalating expression."
template<class T>
constexpr int k(int) {
  return A(42).y;
}

int
test (int i)
{
  int r = g (42) + g(i);
  int t = k<int>(42)
	    + k<int>(i); // { dg-bogus "call to|constant" "" { xfail *-*-* } }
  return r + t;
}

// Just like above, but make the call to id(x) actually a constant.
struct A2 {
  static constexpr int x = 42;
  int y = id(x);
};

template<class T>
constexpr int k2(int) {
  return A2(42).y;
}

int
test2 (int i)
{
  return k2<int>(42) + k2<int>(i);
}
