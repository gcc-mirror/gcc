// PR c++/55944
// { dg-options -std=c++11 }

template<class T>
struct Test
{
  constexpr Test(T val) : value(val) {}
  static void test()
  {
    static constexpr Test<int> x(42); // ICE
  }
  T value;
};

int main()
{
  static constexpr Test<int> x(42); // OK
  Test<double>::test();
}
