//Build don't link:
struct X{
  void i();     // ERROR -
  void i(int);
  int i;        // ERROR - conflict
};
