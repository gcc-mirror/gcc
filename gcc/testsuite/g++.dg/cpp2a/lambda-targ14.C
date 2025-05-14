// PR c++/119345
// { dg-do compile { target c++20 } }

void f(auto... args) {
  [args...]<int... i> {
    (..., [args...] { i; });
  }.template operator()<0>();
}

int main() {
  f();
}
