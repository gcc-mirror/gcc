// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int count;

class A {
public:
  A() {
//    printf("ctor %x\n", (int)this);
    if (count==3)
      throw 1;
    ++count;
    }
  ~A() {
    --count;
//    printf("dtor %x\n", (int)this);
  }
};

main() {
  try {
    A a[5];
  } catch (...) {
    return count;
  }
  return 1;
}
