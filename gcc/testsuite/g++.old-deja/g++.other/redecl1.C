// { dg-do assemble  }
struct X{
  void i();
  void i(int);  // { dg-message "" }
  int i;        // { dg-error "" } conflict
};
