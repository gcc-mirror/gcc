// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

class foo {
public:
  class error {};

  void cause_error(void) { throw error(); }
};

int main(void)
{
  foo f;
  try {
    f.cause_error();
  }
  catch (foo::error&) {
    return 0;
  }
  return 1;
}
