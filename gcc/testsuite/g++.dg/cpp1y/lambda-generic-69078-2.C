// PR c++/69078
// { dg-do run { target c++14 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

template<typename F>
void run( F &&f ) {
  f(nullptr);
}

struct V {
  int i;
};

int main() {
  static V const s={2};
  assert (s.i == 2);
  run([](auto){
      assert (s.i == 2);
    });
}
