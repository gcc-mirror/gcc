// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions -O9" }

void main1() {
  throw 1;
}

int main() {
  try {
    main1();
  } catch (...) {
    return 0;
  }
  return 1;
}
