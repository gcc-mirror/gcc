// PR c++/89660
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

template <typename S> struct D {
  template <typename T> D (D<T> x) : k(&x.foo ()) {}
  S &foo ();
  int *k;
};

D<int> bar ();

struct F {
  D<int> baz () {
    D<F> f = bar ();
    return std::move (*reinterpret_cast<D<int> *> (&f)); // { dg-bogus "redundant move in return statement" }
  }
};
