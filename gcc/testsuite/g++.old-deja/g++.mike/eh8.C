// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

extern "C" int printf(const char *, ...);

int i;

main() {
  try {
    try {
      throw i;
    } catch (char *) {
      return 1;
    }
    return 1;
  } catch (int i) {
    return 0;
  }    
  return 1;
}
