// { dg-do "run" }
// { dg-options "-std=c++0x" }

#include <cassert>
#include <algorithm>

template <typename F, typename A1>
void call(F f, const A1& arg1) {
  f(arg1);
}

int main() {
  int i = 1;
  call(
      [&i] (int j) -> void { i = j; },
      2
  );
  assert(i == 2);

  int A[] = {1, 2, 3, 4};
  int sum = 0;
  std::for_each(A, A+4, [&sum] (int n) -> void { sum += n; });
  assert(sum == 10);

  return 0;
}

