template <typename T> struct A
{
  void foo () const {}
  char A;
};

void bar() { A<void>().foo(); }
