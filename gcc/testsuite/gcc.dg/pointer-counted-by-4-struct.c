/* Test the attribute counted_by for pointer field and its usage in
 * __builtin_dynamic_object_size.  */ 
/* { dg-do run } */
/* { dg-options "-O2" } */
struct A {
  int a;
  char *b;
};
#define PTR_TYPE struct A 
#include "pointer-counted-by-4.c"
