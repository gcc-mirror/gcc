// Build don't link:
struct X{
  void f();        
};

struct Y:X{
  void f(int);
  void f();         // ERROR - conflict
  using X::f;
};                  // ERROR - 
