// { dg-do assemble  }
// 981203 bkoz
// g++/14309
// test for global functions, mf's, and templatized mf's.

static int fooe_1(void) { return 5; }
static int fooe_2(int x = fooe_1()) { return x; }

struct antigua {
  static int& foo_1();
  static int foo_2(int& x = antigua::foo_1());
  static int foo_3(int x = fooe_2());
};

template <typename T>
  struct jamacia {
    static int& foo_1();
    static int foo_2(int& x = antigua::foo_1());
    static int foo_3(int x = fooe_2());
  };

template class jamacia<int>;
