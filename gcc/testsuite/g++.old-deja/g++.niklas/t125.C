// Build don't link: 
// GROUPS passed niklas nested-types static-members
struct A
{
  static void f ();
  struct B
  {
    void h () { f (); }
  };
};
