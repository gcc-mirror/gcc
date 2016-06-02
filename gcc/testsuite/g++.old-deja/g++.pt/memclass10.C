// { dg-do assemble  }

struct S1
{
  template <class T>
  struct S2 {}; // { dg-message "" } previous definition

  template <class T>
  struct S2 {}; // { dg-error "" } redefinition 
};
