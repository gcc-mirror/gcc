// PR c++/106310

template <class T>
struct set{};

template< typename T >
struct Base
{
  template< int > int set(T const &);
};

template< typename T >
struct Derived : Base< T >
{
  void f(T const &arg) {
    this->template set< 0 >(arg);
  }
};
