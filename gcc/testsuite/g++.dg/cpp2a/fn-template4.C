// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

template <typename T> void foo() { }
template <typename T> void bar(int) { }
int main()
{
  foo<float>();
  bar<int>(1);
}
