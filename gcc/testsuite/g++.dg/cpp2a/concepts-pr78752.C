// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

#include <type_traits>

template <class T, class U>
concept bool Same = std::is_same<T, U>::value;

struct test {
  template <Same<int>... Ints>
  void func(Ints... ints) {}
};

int main()
{
  test t;
  t.func(1, 2, 3);
}
