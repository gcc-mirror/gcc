// Special g++ Options: -fexceptions
// excess errors test - XFAIL sparc64-*-elf arm-*-pe

main() {
  try {  
    throw 'a';
  } catch (char a) {
    try {
      throw 'a';
    } catch (int i) {
      return 1;
    } catch (char c) {
      return 0;
    }
  }
  return 1;
}
