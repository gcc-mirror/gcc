// Build don't link: 
// GROUPS passed gb scope
struct D {
  friend class A;
  friend class B;
  friend class C;

  void foo ();
};

void D::foo () { }
