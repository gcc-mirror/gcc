// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

class foo {
public:
  class error {};

  void cause_error(void) { throw "Hello World!"; }
};

int main(void)
{
  foo f;
  try {
    f.cause_error();
  }
  catch (const char cp[]) {
    return 0;
  }
  return 1;
}
