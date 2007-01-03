template<int> struct A
{
  enum { a, b = a };
  void foo(A<b>);
};
