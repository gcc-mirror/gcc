// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

extern "C" int printf(const char *, ...);

main1() {
  throw 1;
}


main() {
  try {
    main1();
  } catch (...) {
    printf("Unwind works!\n");
    return 0;
  }
  return 1;
}
