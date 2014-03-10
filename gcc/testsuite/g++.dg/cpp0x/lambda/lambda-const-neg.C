// { dg-do compile { target c++11 } }

#include <cassert>

template<typename F>
void call(const F& f) { f(); }

int main() {
  call([] () -> void {});
  call([] () mutable -> void {});

  int i = -1;
  call([&i] () -> void { i = 0; });
  assert(i == 0);
  call([i] () -> void { i = 0; }); // { dg-error "" "assignment to non-reference capture in const lambda" }

  return 0;
}

