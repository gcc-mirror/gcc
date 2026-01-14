// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that was in [expr.prim.id.qual].

template <int V>
struct TCls {
  static constexpr int s = V;
  using type = int;
};

constexpr int v1 = [:^^TCls<1>:]::s;
static_assert (v1 == 1);
constexpr int v2 = template [:^^TCls:]<2>::s;
  // OK, template binds to splice-scope-specifier
static_assert (v2 == 2);

constexpr typename [:^^TCls:]<3>::type v3 = 3;
  // OK, typename binds to the qualified name
static_assert (v3 == 3);

constexpr [:^^TCls:]<3>::type v4 = 4; // { dg-error "missing .template.|expected" }
  // error: [:^^TCls:]< is parsed as a splice-expression followed
  // by a comparison operator
