// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

int main () {
  try {
    try {
      throw 1;
    } catch ( char * ) {
    }
  } catch ( int ) {
    return 0;
  }
  return 1;
}
