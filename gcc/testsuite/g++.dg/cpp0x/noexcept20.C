// PR c++/56071
// { dg-do compile { target c++11 } }

class B
{
  template <typename T> friend struct A;
  B() {}
};

template <typename T>
struct A
{
  A() noexcept(noexcept(B())) { }
};

struct C
{
  C()
  {
    static_assert( !noexcept(A<int>()), "" );
  }
};
