// { dg-do assemble  }

class A {
protected:
  int aData;
};
 
class B : public A {
public:
  virtual void func1() { 
    A::aData = 1;
  }
};

class C : virtual public B {
public:
  virtual void func1(void) {
    A::aData = 2;
  }
};
