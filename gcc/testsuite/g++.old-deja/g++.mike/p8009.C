// prms-id: 8009

class A {
public:
  int i;
};

class B : public A {
  B();
};

B::B() : i (-1) {}		// ERROR - 
