// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

struct none { int i[50]; };

class my_ex { } a;

none throw_it() {
  throw 1;
}

int main() {
    try {
      none n = throw_it();
    } catch (int ex) {
      return 0;
    }
}
