/* Testing the fact that the attribute counted_by is not supported in C++.  */
/* { dg-do compile { target c++11 } } */
/* { dg-options "-Wattributes" } */

struct trailing {
  int count;
  int field [[gnu::counted_by (count)]] []; /* { dg-warning "attribute is not supported for C\\+\\+ for now, ignored" } */
};

struct trailing1 {
  int count1;
  [[gnu::counted_by (count)]] int field []; /* { dg-warning "attribute is not supported for C\\+\\+ for now, ignored" } */
};
