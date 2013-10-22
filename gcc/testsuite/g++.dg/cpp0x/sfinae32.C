// PR c++/51989
// { dg-options -std=c++11 }

template <typename T>
struct is_container
{
  template <typename U, typename V = decltype(((U*)0)->begin())>
  static char test(U* u);

  template <typename U> static long test(...);

  enum { value = sizeof test<T>(0) == 1 };
};

int main()
{
  return is_container<void>::value;
}
