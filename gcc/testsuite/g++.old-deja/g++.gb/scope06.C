// { dg-do assemble  }
// GROUPS passed gb scope
struct C {
  struct D {
    int x;
    void foo ();
  };
      const int Ok = 0; // { dg-error "" } initialization forbidden
};

void C::D::foo ()
{
  x = Ok;
}
