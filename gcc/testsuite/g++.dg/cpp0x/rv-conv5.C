// PR c++/113064
// { dg-do compile { target c++11 } }

struct no_copy {
  no_copy() = default;

  no_copy(const no_copy&) = delete;
  no_copy(no_copy&&);

  no_copy& operator=(const no_copy&) = delete;
  no_copy& operator=(no_copy&&);
};

struct A {
  operator no_copy() &;
  operator no_copy&&() && = delete;
};

int main() {
  no_copy nc;
  A a;
  nc = a;
}
