// { dg-do compile }
// Origin: <anthwil at nortelnetworks dot com>
// c++/4933: using sizeof with comma operator as template argument

template<unsigned F>
struct Foo {};

template<typename T>
T makeT();

template<typename T,typename U>
struct Bar
{
  typedef Foo
  <
    sizeof((makeT<T>(), makeT<U>()))
  > Type;
};
