// Build don't link: 
// GROUPS passed niklas nested-types
struct A {
  struct B { void f (); };
  struct C : B { void f () { B::f (); } };
};
