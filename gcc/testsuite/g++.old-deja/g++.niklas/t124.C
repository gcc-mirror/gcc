// { dg-do assemble  }
// GROUPS passed niklas nested-types static-members
struct A
{
  static void f ();
  struct B
  {
    static void g () { f (); }
  };
};
