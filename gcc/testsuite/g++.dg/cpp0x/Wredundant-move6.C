// PR c++/87882
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

struct Foo {
   Foo Bar() {
     return std::move(*this); // { dg-bogus "redundant move in return statement" }
   }
   Foo Baz() {
     return *this;
   }
  int i;
};

void Move(Foo & f)
{
  f = Foo{}.Bar();
}

void NoMove(Foo & f)
{
  f = Foo{}.Baz();
}
