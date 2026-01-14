// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [expr.prim.id.qual].

template<int V>
struct TCls {
  static constexpr int s = V;
  using type = int;
};

int v1 = [:^^TCls<1>:]::s;
int v2 = template [:^^TCls:]<2>::s;	    // OK, template binds to splice-scope-specifier
typename [:^^TCls:]<3>::type v3 = 3;	    // OK, typename binds to the qualified name
template [:^^TCls:]<3>::type v4 = 4;	    // OK, template binds to the splice-scope-specifier
typename template [:^^TCls:]<3>::type v5 = 5; // OK, same as v3
[:^^TCls:]<3>::type v6 = 6;	  // { dg-error "missing .template.|expected" }
