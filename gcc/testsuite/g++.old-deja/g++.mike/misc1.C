// GROUPS passed construct-destruct
/* g++ constructs j 13 times, and destructs it once. */

extern "C" {
  int printf(...);
  void exit(int);
}

void foo() {
}

class C {
  int val;
 public:
  static int count;
  C(int ii) {
    val = ii;
    ++count;
    printf("up\n");
  }
  ~C() {
    --count;
    printf("down\n");
  }
  int operator ++() {
    return ++val;
  }
  operator int() {
    return val;
  }
};

int C::count = 0;

void bar() {
  for (int ii=0; ii<13; ++ii)
    for (C j=1; j<9; ++j)
      foo();
}

int main() {
  bar();
  if (C::count)
    {
      printf("FAIL\n");
      exit(1);
    }
  else
    {
      printf("PASS\n");
    }
  return 0;
}
