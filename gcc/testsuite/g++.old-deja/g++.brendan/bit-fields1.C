// { dg-do assemble  }
// GROUPS passed bit-fields
struct bar {
  int : 2 = 1;// { dg-error "" } .*
};
