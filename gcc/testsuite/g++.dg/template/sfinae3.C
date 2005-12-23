// PR c++/24671
// { dg-options "" }

template<typename> struct A
{
  typedef int X;
  static const int i = 0;
};

template<typename> struct B
{
  B(const B&); // { dg-error "candidate" }
  typedef typename A<char[A<B>::i]>::X Y;
  template<typename T> B(T, Y); // { dg-error "call" }
};

B<int> b(0,0); 
