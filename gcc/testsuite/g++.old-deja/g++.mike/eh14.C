// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

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
