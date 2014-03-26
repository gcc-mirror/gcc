// PR c++/29039

typedef struct S { // { dg-error "reference" "" { target c++11 } }
  int &r; 
}; // { dg-warning "'typedef' was ignored" }


S f () {
  return S (); // { dg-error "reference|deleted" }
}
