// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

extern "C" int printf(const char *, ...);

int i;

int
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
