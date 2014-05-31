// PR c++/57543
// { dg-do compile { target c++11 } }

template< typename > struct X
{
  void foo();
  auto bar() -> decltype( X::foo() );
};

template< typename > struct Y
{
  void foo();
  template< typename >
  auto bar() -> decltype( Y::foo() );
};

template< typename > struct Z
{
  void foo();
  template< typename T >
  auto bar() -> decltype( T::foo() );
};

template< typename > struct K
{
  void foo();
  template< typename T >
  auto bar() -> decltype( T::foo() );
};

template<>
template<>
auto K<int>::bar<K<int>>() -> decltype( K<int>::foo() );

int main()
{
  X<int>().bar();
  Y<int>().bar<double>();
  Z<int>().bar<Z<int>>();
  K<int>().bar<K<int>>();
}
