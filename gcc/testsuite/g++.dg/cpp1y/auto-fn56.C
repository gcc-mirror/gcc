// PR c++/91378
// { dg-do compile { target c++14 } }

struct B
{
  int i;
};

struct C
{
  template <class T> static auto
  g(B b) noexcept(noexcept(b.i)) { }
};

template <class T>
void h(T t)
{
  C::g<int>({});
}
