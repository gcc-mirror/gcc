// { dg-do compile }
// Contributed by David Abrahams <dave at boost-consulting dot com>
// PR c++/14143: Koenig lookup should only look into template arguments only 
//  if the argument is a template-id.

namespace fu
{
  template <class T>
  struct bar
  {
      struct baz {};
  };
}

namespace axe
{
  struct handle {};
  
  template <class T>
  char* f(T&);
}

namespace test
{
  template <class T>
  int f(T const&);
  
  template <class T>
  int g(T x) { return f(x); }
  
  int x = g(fu::bar<axe::handle>::baz());
}
