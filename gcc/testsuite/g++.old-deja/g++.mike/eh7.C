// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

main() {
  if (0)
    throw 1 | 2;
}
