// PR c++/11554
// { dg-options "-Wall" }

struct Y {
  Y ();
  int i1, i2; // { dg-warning "" }
};

Y::Y () : i2(0), i1(0) {} // { dg-warning "" }
