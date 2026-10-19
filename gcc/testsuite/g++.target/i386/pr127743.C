// PR rtl-optimization/127743
// { dg-do run { target sse2_runtime } }
// { dg-options "-O2 -msse2" }
// { dg-require-effective-target c++11 }

#include <vector>

struct C { _Float16 re, im; };

[[gnu::noinline]] std::vector<C> conv(const std::vector<C>& p, float s) {
  std::vector<C> half;
  half.reserve(p.size() + 1);
  half.push_back({p.front().re, 0});
  for (unsigned k = 1; k < p.size(); ++k)
    half.push_back({(_Float16)((float)p[k].re * s), p[k].im});
  half.push_back({p.front().im, 0});   // <-- this store; im must be 0
  return half;
}

int main() {
  std::vector<C> y(2);
  y[0] = {(_Float16)0.5f, (_Float16)0.5f};
  y[1] = {(_Float16)0.5f, (_Float16)0.0f};
  std::vector<C> r = conv(y, 1234.5f);
  if (r.back().im != 0)
    __builtin_abort();
}
