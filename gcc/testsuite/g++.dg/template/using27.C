// PR c++/37140

struct X
{
  typedef int nested_type;
};

template <class T>
struct A
{
  typedef X type;
};

template <class T>
struct B : A<T>
{
  using typename A<T>::type;
  typename type::nested_type x;
};

template <class T> 
struct C : B<T>
{
  using typename B<T>::type;
  typename type::nested_type y;
};

struct D : C<int>
{
  using C<int>::type;
  type::nested_type z;
};

