/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-devirt"  } */
struct Interface {
  virtual ~Interface() {}
  virtual void virtualFunc() = 0;
  virtual void virtualFunc2() = 0;
};

struct Concrete : Interface {
  int counter_;
  Concrete() : counter_(0) {}
  void virtualFunc() { counter_++; }
  void virtualFunc2() { counter_++; }
};

void test(Interface &c) {
  c.virtualFunc();
  c.virtualFunc2();
}
/* { dg-final { scan-ipa-dump "2 speculatively devirtualized" "devirt"  } } */
