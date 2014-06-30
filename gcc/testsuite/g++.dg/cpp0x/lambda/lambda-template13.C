// PR c++/61566
// { dg-do compile { target c++11 } }

struct function
{
  template < typename _Functor>
  function (_Functor);
};

struct C
{
  template <typename T>
  void foo (T, function = [] {});
};

void bar ()
{
  C c;
  c.foo (1);
}
