// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*
// prms-id: 7912

int count = 0;

class Foo {
public:
  Foo() { ++count; };
  Foo(const Foo&) { ++count; };
  ~Foo() { --count; };
};


main()
{
  try {
    throw Foo();
  }
  catch (Foo object) {
  }
  return count;
}
