// { dg-do assemble  }
extern "C" {
  class A {
  public:
    void a();
  };
}

void A::a() {}
