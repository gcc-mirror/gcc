// { dg-options "-std=c++0x" }
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
