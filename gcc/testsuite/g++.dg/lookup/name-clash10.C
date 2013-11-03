// PR c++/38313

struct foo { };
struct bar { };

struct baz {
  static foo (bar)();
};
