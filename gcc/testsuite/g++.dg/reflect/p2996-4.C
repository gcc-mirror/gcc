// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [expr.prim.splice].

struct S { static constexpr int a = 1; };
template <typename> struct TCls { static constexpr int b = 2; };

constexpr int c = [:^^S:]::a;                   // [:^^S:] is not an expression
static_assert(c == 1);

constexpr int d = template [:^^TCls:]<int>::b;  // template [:^^TCls:]<int> is not
                                                // an expression
static_assert(d == 2);

template <auto V> constexpr int e = [:V:];   // splice-expression
constexpr int f1 = e<^^S::a>;  // splice-expression
constexpr int f2 = template [:^^e:]<^^S::a>;  // splice-expression
static_assert(f1 == 1);
static_assert(f2 == 1);

constexpr auto g = typename [:^^int:](42);
  // [:^^int:] forms part of a type, not a splice-expression
static_assert(g == 42);

constexpr auto h = ^^g;
constexpr auto i = e<[:^^h:]>;	  // { dg-error "unparenthesized splice|invalid" }
constexpr auto j = e<([:^^h:])>;
