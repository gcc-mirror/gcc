// PR c++/40139

template<int> struct A
{
  static int i;
};

template<int N> int A<N>::i = { A::~A }; // { dg-error "non-static member function" }

template class A<0>;

struct X { };

int i1 = X::~X;			// { dg-error "non-static member function" }
int i2 = &X::~X;		// { dg-error "address of destructor" }
int i3 = &A<0>::~A;		// { dg-error "address of destructor" }
