// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

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
