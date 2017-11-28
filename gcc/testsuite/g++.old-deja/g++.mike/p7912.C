// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }
// prms-id: 7912

int count = 0;

class Foo {
public:
  Foo() { ++count; };
  Foo(const Foo&) { ++count; };
  ~Foo() { --count; };
};


int
main()
{
  try {
    throw Foo();
  }
  catch (Foo object) {
  }
  return count;
}
