// Special g++ Options: -fexceptions -g
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

class zeroset {
public:
  ~zeroset () { }
};

int main () {
  zeroset a;
  try {
    ;
  } catch( zeroset ) {
  }
}
