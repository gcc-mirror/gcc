// { dg-do compile }
// { dg-options -std=c++17 }

#define SA(X) static_assert((X),#X)

template<typename>
constexpr int
foo ()
{
  constexpr int a[] = { 1, 2, 3, 4, 5 };
  int i = 0;
  auto j = [&] {
    for (auto x : a)
      i++;
    return i;
  }();
  return j;
}

SA (foo<int>() == 5);
