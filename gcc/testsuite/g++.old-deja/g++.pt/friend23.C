// Build don't link:

template <class T = int> // ERROR - original definition
struct S
{ // ERROR - redefinition of default arg
  template <class U = int>
  friend class S;
};

template struct S<int>;
