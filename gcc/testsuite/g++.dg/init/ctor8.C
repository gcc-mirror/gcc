// PR c++/29039

typedef struct S { // { dg-error "reference" }
  int &r; 
};

S f () {
  return S (); // { dg-error "synthesized" }
}


