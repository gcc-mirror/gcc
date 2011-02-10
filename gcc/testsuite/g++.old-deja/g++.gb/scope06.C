// { dg-do assemble  }
// { dg-prune-output "non-static data member initializers" }
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
  // { dg-prune-output "from this location" }
  x = Ok;
}
