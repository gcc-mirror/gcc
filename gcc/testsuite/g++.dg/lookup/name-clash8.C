// PR c++/38278

struct foo { };
void bar();

struct baz {
  static foo (bar)();
};
