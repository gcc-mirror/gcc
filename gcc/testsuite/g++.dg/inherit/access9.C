// PR c++/66957

class BaseClass {
protected:
  static int x;
};

struct DerivedA : BaseClass { };

struct DerivedB : BaseClass {
  DerivedB() {
    (void) DerivedA::x;
  }
};
