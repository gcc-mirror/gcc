// PR c++/58954
// { dg-require-effective-target c++11 }

template<class T>
T&& declval();

template<class T>
struct foo_argument
{
  template<class Ret, class C, class Arg>
  static Arg test(Ret (C::*)(Arg));

  typedef decltype(test(&T::template foo<>)) type;
};

template<class T, class>
struct dependent { typedef T type; };

template<class T>
struct base
{
  template<class Ignore = void>
  auto foo(int i) -> decltype(declval<
    typename dependent<T&, Ignore>::type
  >().foo_impl(i));
};

struct derived : base<derived>
{
  friend struct base<derived>;
private:
  int foo_impl(int i);
};

int main()
{
  foo_argument<derived>::type var = 0;
  return var;
}
