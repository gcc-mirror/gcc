// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

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
