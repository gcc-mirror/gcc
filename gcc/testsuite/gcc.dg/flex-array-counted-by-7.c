/* Testing the correct usage of attribute counted_by: _BitInt  */   
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

struct trailing_array {
  _BitInt(24) count; 
  int array[] __attribute ((counted_by (count)));
};
