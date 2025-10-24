/* Testing the correct usage of attribute counted_by for pointer to void.  */
/* { dg-do compile } */
/* { dg-options "-O0 -Wpointer-arith" } */

struct pointer_array {
  int count;
  void *array __attribute__ ((counted_by (count))); /* { dg-warning "attribute is used for a pointer to void" } */
}; 
