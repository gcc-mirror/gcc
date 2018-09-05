// PR c++/87080
// { dg-do compile { target c++11 } }
// { dg-options "-Wpessimizing-move" }

struct a {
  template<typename b> a &operator<<(b);
};
a c();
template<typename>
a fn2()
{
  int d = 42;
  return c() << d;
}
