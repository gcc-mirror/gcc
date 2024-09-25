// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template <class T, class U>
concept Same = __is_same(T, U);

struct test {
  void func(Same<int> auto... ints) {}
};

void func(Same<int> auto... ints) {}

int main()
{
  test t;
  t.func(1, 2, 3);
  func(1, 2, 3);

  t.func(1, 2, ""); // { dg-error "no match" }
  func(1, 2, ""); // { dg-error "no match" }
}
