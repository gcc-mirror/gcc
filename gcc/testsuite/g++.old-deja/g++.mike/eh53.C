// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions -g" }

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
