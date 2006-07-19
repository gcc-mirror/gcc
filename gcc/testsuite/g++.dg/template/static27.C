// PR c++/28235

template<typename> struct A
{
  static const bool i = true;
  template<bool = i> struct B {};
  B<> b;
};

void f() {
  A<int> a1, a2;
  a1.b = a2.b;
}
