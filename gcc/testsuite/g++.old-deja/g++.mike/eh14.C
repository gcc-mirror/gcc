// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

class arghh {
public:
  int n;
  arghh (int v) { n = v; }
};

int main () {
  try {
    throw arghh (11);
  }
  catch (arghh& a) {
    if (a.n != 11)
      return 1;
  }
  try {
    throw arghh (22);
  }
  catch (arghh& a) {
    if (a.n != 22)
      return 2;
  }
}
