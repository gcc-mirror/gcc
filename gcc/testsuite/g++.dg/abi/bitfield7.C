// { dg-do compile }
// { dg-options "-Wabi" }

union U { // { dg-warning "ABI" }
  int i: 4096; // { dg-warning "exceeds" }
};

