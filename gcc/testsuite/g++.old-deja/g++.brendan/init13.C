// { dg-do assemble  }
// GROUPS passed initialization
struct A {
  operator int ();
};
 
int i = A();
