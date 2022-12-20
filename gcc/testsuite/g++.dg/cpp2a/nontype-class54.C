// PR c++/104278
// { dg-do compile { target c++20 } }

struct foo {
  int value;
  constexpr foo modify() const { return { value + 1 }; }
};

template<foo f, bool Enable = f.value & 1>
struct bar {
  static void run() { }
};

template<foo f>
struct qux {
  static void run() {
    bar<f.modify()>::run();
  }
};

int main() {
  qux<foo{}>::run();
}
