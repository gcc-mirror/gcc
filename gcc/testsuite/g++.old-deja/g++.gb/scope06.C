// Build don't link: 
// GROUPS passed gb scope
struct C {
  struct D {
    int x;
    void foo ();
  };
      const int Ok = 0; // ERROR - initialization forbidden
};

void C::D::foo ()
{
  x = Ok;
}
