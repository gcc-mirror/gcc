// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

int x = 42;
template<typename T>
constexpr T two = 2;

template<typename T>
constexpr T foo (T t) { return t; }

void bar () { }

struct S { };

template<typename>
struct ST { };

template <int P1, const int &P2> void fn() {}

static constexpr int p[2] = {1, 2};
constexpr auto spec = ^^fn<p[0], p[1]>;

void
g ()
{
  int i1 = [: ^^x :];
  int i2 = template [: ^^x :];    // { dg-error "reflection .x. not usable in a template splice" }
  int i3 = [: ^^two<int> :];
  int i4 = template [: ^^two<int> :]; // { dg-error "reflection .two<int>. not usable in a template splice" }
  int i5 = [: ^^foo :](42);	      // { dg-error "reflection .foo. not usable in a splice expression" }
  int i6 = template [: ^^foo :](42);
  int i7 = [: ^^foo<int> :](42);
  int i8 = template [: ^^foo<int> :](42);   // { dg-error "reflection .foo<int>. not usable in a template splice" }
  int i9 = [: ^^foo :]<int>(42);	    // { dg-error "reflection .foo<int>. not usable in a splice expression with template arguments" }
  int i10 = template [: ^^foo :]<int>(42);
  int i11 = template [: ^^bar :]<int>(42);  // { dg-error "no matching function for call" }
  int i12 = [: ^^two :]<int>;		    // { dg-error "reflection .two<int>. not usable in a splice expression with template arguments" }
  int i13 = template [: ^^two :]<int>;

  [: ^^ST :]<int> c1;  // { dg-error "reflection .ST<int>. not usable in a splice expression with template arguments" }
  [: ^^S :]<int> c2;   // { dg-error "not a template|reflection not usable in a splice expression with template arguments" }
  [: ^^bar :]<int>();	// { dg-error "reflection .bar<int>. not usable in a splice expression with template arguments" }

  auto x1 = [: ^^ST :]<int>{};	  // { dg-error "reflection .ST<int>. not usable in a splice expression with template arguments" }
  auto x2 = template [: ^^ST :]<int>{};	// { dg-error "expected a reflection of an expression" }
  auto x3 = typename [: ^^ST :]<int>{};
}
