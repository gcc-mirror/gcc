// { dg-do assemble  }
// GROUPS passed niklas nested-types static-members
struct A
{
  static void f ();
  struct B
  {
    void g () {}
    void h () {}
  };
};
