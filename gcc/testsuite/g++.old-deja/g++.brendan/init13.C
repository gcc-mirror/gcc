// Build don't link: 
// GROUPS passed initialization
struct A {
  operator int ();
};
 
int i = A();
