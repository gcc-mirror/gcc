// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

extern "C" int printf(const char *, ...);

void main1() {
  throw 1;
}


int main() {
  try {
    main1();
  } catch (...) {
    printf("Unwind works!\n");
    return 0;
  }
  return 1;
}
