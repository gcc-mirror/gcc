// { dg-do assemble  }
struct X{
  void i();
  void i(int);  // { dg-error "" } 
  int i;        // { dg-error "" } conflict
};
