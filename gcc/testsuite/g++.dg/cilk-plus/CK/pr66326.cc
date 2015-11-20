/* { dg-options "-fcilkplus" } */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-fcilkplus -lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

#include <vector>
#include <random>

template <class T>
void do_not_optimize_away(T&& x) {
  asm volatile("" : "+r"(x));
}

const int N = 1'000'000;

auto compute() {
  std::vector<double> v(N);
  auto rng = std::mt19937{std::random_device{}()};
  std::uniform_real_distribution<double> dist(0, 1);
  for (int i = 0; i < N; ++i) v[i] = std::log(std::sqrt(dist(rng)));
  return v;
}

int main() {
  std::vector<double> v1, v2, v3;
  _Cilk_spawn [&] { v1 = compute(); }();
  _Cilk_spawn [&] { v2 = compute(); }();
  v3 = compute();
  do_not_optimize_away(v1.data());
  do_not_optimize_away(v2.data());
  do_not_optimize_away(v3.data());
  return 0;
}
