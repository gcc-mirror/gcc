// { dg-do assemble  }

template <class T = int> // { dg-error "" } original definition
struct S
{ // { dg-error "" } redefinition of default arg
  template <class U = int>
  friend class S;
};

template struct S<int>;
