// PR c++/88692
// { dg-do compile { target c++11 } }
// { dg-options "-Wredundant-move" }

// Define std::move.
namespace std {
  template<typename _Tp>
    struct remove_reference
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp   type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp   type; };

  template<typename _Tp>
    constexpr typename std::remove_reference<_Tp>::type&&
    move(_Tp&& __t) noexcept
    { return static_cast<typename std::remove_reference<_Tp>::type&&>(__t); }
}

struct X {
    X f() && {
        return std::move(*this); // { dg-bogus "redundant move in return statement" }
    }

    X f2() & {
        return std::move(*this); // { dg-bogus "redundant move in return statement" }
    }

    X f3() {
        return std::move(*this); // { dg-bogus "redundant move in return statement" }
    }
};

struct S { int i; int j; };

struct Y {
  S f1 (S s) {
    return std::move (s); // { dg-warning "redundant move in return statement" }
  }

  S f2 (S* s) {
    return std::move (*s); // { dg-bogus "redundant move in return statement" }
  }

  S f3 (S** s) {
    return std::move (**s); // { dg-bogus "redundant move in return statement" }
  }
};
