// PR c++/109396
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

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
struct A {
    int i_;

    A(int) : i_(i_) { } // { dg-warning "itself" }
    A(int, int) : i_(this->i_) { } // { dg-warning "itself" }
};

struct B {
    int i_;

    B(int) : i_(std::move(i_)) { }  // { dg-warning "itself" }
    B(int, int) : i_(std::move(this->i_)) { } // { dg-warning "itself" }
};

