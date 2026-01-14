// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from [basic.splice].

using info = decltype(^^int);

constexpr int v = 1;
template<int V> struct TCls {
  static constexpr int s = V + 1;
};

// OK, a splice-specialization-specifier with a splice-expression as a template argument
using alias = [:^^TCls:]<([:^^v:])>;

static_assert(alias::s == 2);

// error: < means less than
auto o1 = [:^^TCls:]<([:^^v:])>();  // { dg-error "reflection .TCls<1>. not usable" }
// OK, o2 is an object of type TCls<1>
auto o2 = typename [:^^TCls:]<([:^^v:])>();

consteval int bad_splice(info v) {
  return [:v:];	  // { dg-error ".v. is not a constant expression" }
}
