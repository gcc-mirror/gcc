// { dg-do assemble  }
namespace foo {

  template <class T>
  class x {};

}

foo::x<int> y;
