// PR c++/17826

struct A
{
  template<typename> static int foo();
};

template<int> struct B {};

template<typename T> void bar()
{
  B<sizeof A::foo<T>()> b1;
  B<sizeof A::foo<T>()> b2;
}
