// { dg-do assemble  }

struct S1
{
  template <class T>
  struct S2 {}; // { dg-error "" } previous definition

  template <class T>
  struct S2 {}; // { dg-error "" } redefinition 
};
