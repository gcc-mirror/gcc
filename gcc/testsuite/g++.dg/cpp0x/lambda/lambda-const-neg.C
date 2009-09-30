// { dg-options "-std=c++0x" }

#include <cassert>

template<typename F>
void call(const F& f) { f(); }	// { dg-error "discards qualifiers" }

int main() {
  call([] () -> void {});
  call([] () mutable -> void {}); // { dg-message "" "`f' does not have const `operator()'" }

  int i = -1;
  call([&i] () -> void { i = 0; });
  assert(i == 0);
  call([i] () -> void { i = 0; }); // { dg-error "" "assignment to non-reference capture in const lambda" }

  return 0;
}

