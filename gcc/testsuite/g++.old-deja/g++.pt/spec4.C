// { dg-do assemble  }

template <class T>
struct S {};

template <>
struct S<int>
{
  void f();
  void g();
};

void S<int>::f() {}

template <>
void S<int>::g() {} // { dg-error "" } does not match any template declaration
