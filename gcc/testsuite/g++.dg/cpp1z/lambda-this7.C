// PR c++/95193
// { dg-do compile { target c++17 } }

struct X {
  void foo() const {
    auto GL1 = [*this](auto a) {
    };

    GL1("abc");
  }
};
