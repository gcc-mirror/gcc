// Build don't link:

struct S1
{
  template <class T>
  struct S2 {}; // ERROR - previous definition

  template <class T>
  struct S2 {}; // ERROR - redefinition 
};
