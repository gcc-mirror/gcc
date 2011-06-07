// PR c++/24671
// { dg-do compile }

template<typename> struct A
{
  typedef int X;
  static const int i = 0;
};

template<typename> struct B
{
  B(const B&);
  typedef typename A<char[A<B>::i]>::X Y; // { dg-error "forbids zero-size array" }
  template<typename T> B(T, Y);
};

B<int> b(0,0); // { dg-message "required from here" }
