// PR c++/16870

struct A
{
  int operator[](int) const;
};

template<int> A foo();

A bar(A(*)());

template<int> int baz() { return (bar(&foo<0>))[0]; }

template int baz<0>();
