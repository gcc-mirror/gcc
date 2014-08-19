// { dg-final { scan-assembler-not "weak" } }

template <class T> struct A { static int i; };
template<> int A<int>::i = 42;
