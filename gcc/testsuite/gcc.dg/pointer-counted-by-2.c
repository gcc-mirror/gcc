/* Testing the correct usage of attribute counted_by for pointer: _BitInt  */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

struct pointer_array {
  _BitInt(24) count; 
  int *array __attribute__ ((counted_by (count)));
  int *array1 __attribute__ ((counted_by (count1)));
  _BitInt(24) count1; 
};
