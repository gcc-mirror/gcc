// PR c++/67906
// { dg-do compile { target c++11 } }
// { dg-options "-Wall -Wextra" }

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
  X() { }
  X(const X&) { }
  X(const X&&) { }
};

const X x;
const X y = std::move(x);
