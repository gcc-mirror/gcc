// PR c++/120757
// { dg-do compile { target c++14 } }

template <typename> struct A
{
  auto foo() {}
};

auto bar(void (A<int>::*)()) {}

template <int> auto baz()
{
  bar(&A<int>::foo);
}

int main()
{
  baz<0>();
  return 0;
}
