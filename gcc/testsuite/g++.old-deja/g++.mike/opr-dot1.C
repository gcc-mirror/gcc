// { dg-do assemble  }

typedef struct base1 {
  int x;
} base1_t;

typedef struct base2 {
  int x;
} base2_t;

class derived1 : public base1 {
};

class derived2 : public derived1, public base2 {
};

struct test {
  derived2& fails;
  void test1() {
    fails.base1::x = 5;
  }
};
