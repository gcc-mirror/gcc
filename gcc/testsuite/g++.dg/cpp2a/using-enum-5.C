// Examples from P1099R5
// { dg-do compile { target c++20 } }

namespace my_lib {

  enum class errcode
    {
     SUCCESS = 0,
     ENOMEM = 1,
     EAGAIN = 2,
     ETOOSLOW = 3
    };
  using enum errcode; // import enumerators into namespace
}

namespace NS {
  my_lib::errcode get_widget() {
    using namespace my_lib;
    return ETOOSLOW; // works, and conversions to int don't.
    int i = ETOOSLOW;		// { dg-error "" }
  }
}

enum class rgba_color_channel { red, green, blue, alpha};

const char * to_string(rgba_color_channel channel) {
  switch (channel) {
    using enum rgba_color_channel;
  case red:   return "red";
  case green: return "green";
  case blue:  return "blue";
  case alpha: return "alpha";
  }
  return nullptr;
}

namespace ns {
  struct E_detail {
    enum E { e1, e2 };
    friend void swap(E&, E&);  // adl-only swap in the only associated scope of the enum
  };
  using E = E_detail::E;  // import E into ns
  using enum E;           // expose the enumerators of E in ns. Also note the direct reference to E.
}

int main() {
  auto x = ns::e1;
  auto y = ns::e2;
  swap(x, y); // finds the swap in the associated struct
}

namespace N0 {
  enum E { x };
  struct S {
    enum H { y };
    enum class K { z };
    using E::x; // OK, introduces x into S
    using E::x; // { dg-error "" } redeclaration in class scope
    using H::y; // { dg-error "" } redeclaration in class scope
    using K::z; // OK, introduces z into S
  };
  namespace NS {
    enum H { y };
    enum class K { z };
    using E::x; // OK, introduces x into NS
    using E::x; // OK, just a redeclaration of the same entity
    using H::y; // OK, redeclaration of the same entity
    using K::z; // OK, introduces z into NS
  };
}
namespace N1 {
  struct S {
    enum E { x };
    enum class EC { y };
    using EC::y;
  };

  void f() {
    using S::x; // OK
    x; // resolves to S::E::x;
    using S::y; // OK
    y; // resolves to S::EC::y;
  }
}

namespace N2 {
  enum class E { a, b, c };
  using E::a, E::b, E::c; // OK, imports all three
  auto x = (a,b,c);
}

namespace N3 {
  struct B {
    enum class E { x };
  };
  enum class H { y };
  struct C : B {
    using enum B::E; // OK, introduces E::x into C
    using enum H; // OK, introduces y into C. Does not introduce H
  };
  auto i = C::y;  // OK
  C::H h;	  // { dg-error "" }
}

namespace N4 {
  enum class button { up, down };
  struct S {
    using button::up;
    button b = up; // OK
  };
}

namespace N5 {
  enum class fruit { orange, apple };
  struct S {
    using enum fruit; // OK, introduces orange and apple into S
  };
  void f() {
    S s;
    s.orange; // OK, names fruit::orange
    S::orange; // OK, names fruit::orange
  }
}

namespace N6 {
  enum class fruit { orange, apple };
  enum class color { red, orange };
  void f() {
    using enum fruit; // OK
    using enum color; // { dg-error "" } color::orange and fruit::orange conflict
  }
}
