// PR c++/50837
// { dg-options "-std=c++11" }

template<class T>
struct z
{
  static constexpr bool test_constexpr()
  {
    return true;
  }

  static void test()
  {
    static_assert(test_constexpr(), "test1");
  }
};

int main()
{
  z<int>::test();
}
