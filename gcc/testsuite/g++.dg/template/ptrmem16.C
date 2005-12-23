// PR c++/25369
// { dg-do link }

template <typename> struct A 
{
  void foo() {}
};

void bar(void (A<int>::*)()) {}

template <int> void baz()
{
  bar(&A<int>::foo);
}

int main()
{
  baz<0>();
  return 0;
}
