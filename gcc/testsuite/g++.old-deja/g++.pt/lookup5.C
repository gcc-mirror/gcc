// Build don't link:

struct B { 
  int i;
};

struct D: public B {
  int i;
};

template <class T>
struct D2 : public D {
  void f() { i = 3; }
};
