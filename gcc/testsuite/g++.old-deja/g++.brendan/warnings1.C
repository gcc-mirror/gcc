// { dg-do assemble  }
// GROUPS passed warnings
// there should be a warning about foo only defining private methods
class foo { // { dg-error "" } .*
  int bar();
};
