// { dg-do assemble  }
class A {
  public:
    static int a;
};

class B : public A {
  public:
    static int b;
};

int B::a; // { dg-error "" } 
