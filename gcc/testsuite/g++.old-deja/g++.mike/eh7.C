// Special g++ Options: -fexceptions
// excess errors test - XFAIL sparc64-*-elf arm-*-pe

main() {
  if (0)
    throw 1 | 2;
}
