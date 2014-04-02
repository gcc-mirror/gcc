// { dg-do compile { target c++11 } }
template<int... N> struct A
{
  static void foo()
  {
    int i = N; // { dg-error "not expanded|N" }
  }
};

void bar()
{
  A<0>::foo();
}
