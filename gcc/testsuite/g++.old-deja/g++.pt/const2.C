// This test should get a linker error for the reference to A<int>::i.
// An XPASS on this test is really a FAIL.
// excess errors test - XFAIL *-*-*

template <class T> struct B { static const int i = 3; };
template <class T> struct A { static const int i = B<T>::i; };
const int *p = &A<int>::i;

int main(){}
