// Special g++ Options: -Woverloaded-virtual
// Build don't link:

struct A {
  virtual void f(); // WARNING - hidden 
};

struct B: public A {
  void f(int); // WARNING - by this
};
