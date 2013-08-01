// { dg-do compile }

namespace a
{
  void f(int);
}

namespace b
{
  void f(int); // { dg-message "previous" }
  void g()
  {
    f (3);
  }
  using a::f; // { dg-error "conflicts" }
}
