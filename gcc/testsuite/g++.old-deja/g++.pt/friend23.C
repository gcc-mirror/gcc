// { dg-do assemble  }

template <class T = int> // { dg-message "note: original definition" }
struct S
{ 
  template <class U = int>
  friend class S; // { dg-error "redefinition of default argument" }
};

template struct S<int>;
