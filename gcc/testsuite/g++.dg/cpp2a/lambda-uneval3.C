// { dg-do compile { target c++20 } }

template <int N> void foo(const char (*s)[([]{}, N)]) {}
template <class T> void spam(decltype([]{}) (*s)[sizeof(T)]) {}

int main()
{
  foo<1>(nullptr);
  spam<char>(nullptr);
}

// { dg-final { scan-assembler-not "weak.*_Z" } }
