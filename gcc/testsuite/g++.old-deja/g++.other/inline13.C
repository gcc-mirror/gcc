// Build don't link:
// Origin: Alexandre Oliva <aoliva@cygnus.com>

struct foo {
  inline void bar();
  foo();
};

inline void foo::bar() {
  switch (0) {
  case 0:
    break;
  }
}

foo::foo() {
  bar();
}
