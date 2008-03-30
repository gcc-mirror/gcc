// PR c++/35578
// Check position of error message
// { dg-do compile }

int i;

friend  // { dg-error "friend" }
  void foo();
