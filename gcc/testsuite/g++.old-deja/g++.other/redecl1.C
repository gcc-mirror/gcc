//Build don't link:
struct X{
  void i();
  void i(int);  // ERROR - 
  int i;        // ERROR - conflict
};
