// { dg-do compile }
// { dg-options "-Wabi -fabi-version=1" }

union U { // { dg-warning "ABI" }
  int i: 4096; // { dg-warning "exceeds" }
};

