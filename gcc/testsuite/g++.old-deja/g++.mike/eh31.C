// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int count;

class Foo {
public:
  Foo() { ++count; }
  Foo(const Foo&) { ++count; }
  ~Foo() { --count; }
};


main() {
  try {
    throw Foo();
  }
  catch (Foo& object) {
    if (count == 1)
      return 0;
  }
  return 1;
}
