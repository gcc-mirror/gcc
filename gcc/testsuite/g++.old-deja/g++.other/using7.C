// Build don't link:

class A {
protected:
  static void f() {};
};

class B : A {
public:
  using A::f;
  void g() {
    f();
    A::f();
  }
  struct C {
    void g() {
      f();
      A::f();
    }
  };
};
