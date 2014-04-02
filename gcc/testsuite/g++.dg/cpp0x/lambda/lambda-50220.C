// PR c++/50220
// { dg-do compile { target c++11 } }

template<typename Foo> struct Foobar {};

void foobar(const Foobar<void>& obj)
{
  [obj](){}();
}
