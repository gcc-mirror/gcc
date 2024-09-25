/* Testing the fact that the attribute counted_by is not supported in C++.  */
/* { dg-do compile } */
/* { dg-options "-Wattributes" } */

int size;
int x __attribute ((counted_by (size))); /* { dg-warning "attribute is not supported for C\\+\\+ for now, ignored" } */

struct trailing {
  int count;
  int field[] __attribute ((counted_by (count))); /* { dg-warning "attribute is not supported for C\\+\\+ for now, ignored" } */
};
