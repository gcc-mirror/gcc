// PR c++/40139

template<int> struct A
{
  static int i;
  ~A();
};

template<int N> int A<N>::i = { A::~A }; // { dg-error "36:invalid use of non-static member function" }

template class A<0>;

struct X { };

int i1 = X::~X;			// { dg-error "13:invalid use of non-static member function" }
int i2 = &X::~X;		// { dg-error "10:taking address of destructor" }
int i3 = &A<0>::~A;		// { dg-error "10:taking address of destructor" }
