// { dg-do assemble  }
// GROUPS passed gb scope
struct C {
  void foo (int);
  void foo (char);

  struct D {
    void foo ();
  };
};

void C::D::foo () { }
