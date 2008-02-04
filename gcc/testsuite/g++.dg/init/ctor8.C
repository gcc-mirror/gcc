// PR c++/29039

typedef struct S {
  int &r; 
}; // { dg-warning "'typedef' was ignored" }

S f () {
  return S (); // { dg-error "reference" }
}
