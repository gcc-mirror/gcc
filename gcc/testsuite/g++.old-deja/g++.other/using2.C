// { dg-do assemble  }
struct X{
  void f();        
};

struct Y:X{
  void f(int);
  void f();
  using X::f;
};
