// { dg-do run  }
// prms-id: 2960

extern "C" int printf(const char *, ...);

class Test0 {
public:
  virtual void f0() { }      // works fine if this virtual removed
};

class Test1 : public Test0 {
public:
  void f1() { f2(); }		// generates bus error here
  virtual void f2() { printf("Test1::f2\n"); }
};

class Test2 {
public:
  virtual void f3() { }
};

class Test3 : public Test2, public Test1 {    // works fine if Test1 first
public:
  virtual ~Test3() { f1(); }                       // calling f2 directly works
  virtual void f2() { printf("Test3::f2\n"); }
};

int main() {
    Test3 t3;
    return 0;
}
