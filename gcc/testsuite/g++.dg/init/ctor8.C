// PR c++/29039

typedef struct S { // { dg-error "reference" "" { target c++11 } }
  int &r; 
}; // { dg-warning "1:'typedef' was ignored" "" { target *-*-* } 3 }


S f () {
  return S (); // { dg-error "reference|deleted" }
}
