// PR c++/53763
// { dg-do compile { target c++11 } }

template<typename TYPE>
struct A
{
  static int a(TYPE value)
  {
    return value;
  }
};

template<typename... ARGS>
struct B
{
  static int b(ARGS...)
  {
    return 0;
  }
};

int main()
{
  int x = B<decltype(A<int>::a(1))>::b(A<int>::a(1));
  int y = B<decltype(A     ::a(2))>::b(A<int>::a(2)); // { dg-error "template" }
  return x + y;
}
