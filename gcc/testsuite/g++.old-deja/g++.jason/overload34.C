// { dg-do assemble  }
// PRMS ID: 8010

class X {
  int & flag;
public:
  void f(){ flag++ ; }
};
