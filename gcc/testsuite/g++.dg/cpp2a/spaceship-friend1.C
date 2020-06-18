// P2085, separate definition of defaulted comparisons
// { dg-do compile { target c++20 } }

namespace X {

  struct A {
    int i;
    friend constexpr bool operator==(const A&,const A&);
  };

  inline constexpr bool operator==(const A&,const A&)=default;

  static_assert (A() == A());

}

namespace Y {

  struct A {
    int i;
    // friend bool operator==(const A&,const A&);
  };

  inline bool operator==(const A&,const A&)=default; // { dg-error "not a friend" }

}
