// Build don't link:

class A;

class B {
public:
  B(); 
private:
  A a;  // ERROR - 
};

class A { };
B::B() { }
