// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

void foo() {
  int i;
  i = 42;
  throw i;
}

void ee(int *);

void bar() {
  int i = 2;
  ee(&i);
}

void ee(int *) { }

main() {
  try {
    foo();
    return 3;
  } catch (int& i) {
    bar();
    return i != 42;
  }
  return 2;
}
