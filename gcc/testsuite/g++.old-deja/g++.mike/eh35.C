// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int
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
