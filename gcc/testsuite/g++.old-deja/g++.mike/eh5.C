// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

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
