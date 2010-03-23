// { dg-do "run" }
// { dg-options "-std=c++0x" }

#include <cassert>

template<typename F>
void call(F f) { f(); }

int main() {
  call([] () -> void {});
  call([] () mutable -> void {});

  int i = -1;
  call([i] () mutable -> void { i = 0; });
  assert(i == -1);

  return 0;
}

