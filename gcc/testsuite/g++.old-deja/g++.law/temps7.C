// Build don't link: 
// GROUPS passed temps
// excess errors test - XFAIL *-*-*

template <class B >
class A {
public:
  class C {};
};

template class A<int>::C;

