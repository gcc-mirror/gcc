// PR c++/34491

template<typename> struct A;

template<0> struct A<int> // { dg-error "expected|template|anonymous" }
{
  static const int i = 0;
};

int n = A<int>::i; // { dg-error "incomplete type" }
