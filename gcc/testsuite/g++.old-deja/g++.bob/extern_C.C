// Build don't link: 
extern "C" {
  class A {
  public:
    void a();
  };
};

void A::a() {}
