// Special g++ Options: -fexceptions -O9
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

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
