// { dg-do assemble  }
// { dg-options "-Wctor-dtor-privacy" }
// GROUPS passed warnings
// there should be a warning about foo only defining private methods
class foo { // { dg-warning "private" }
  int bar();
};
