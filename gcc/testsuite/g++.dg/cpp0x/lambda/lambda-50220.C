// PR c++/50220
// { dg-options -std=c++0x }

template<typename Foo> struct Foobar {};

void foobar(const Foobar<void>& obj)
{
  [obj](){}();
}
