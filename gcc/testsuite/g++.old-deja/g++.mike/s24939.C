// { dg-do assemble  }

class A;

class B {
public:
  B(); 
private:
  A a;  // { dg-error "" } 
};

class A { };
B::B() { }
