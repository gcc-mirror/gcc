// Build don't link:

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
void S<int>::g() {} // ERROR - does not match any template declaration
