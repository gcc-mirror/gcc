// Build don't link: 
namespace foo {

  template <class T>
  class x {};

}

foo::x<int> y;
