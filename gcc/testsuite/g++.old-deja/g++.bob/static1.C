// Build don't link: 
class A {
  public:
    static int a;
};

class B : public A {
  public:
    static int b;
};

int B::a; // ERROR - 
