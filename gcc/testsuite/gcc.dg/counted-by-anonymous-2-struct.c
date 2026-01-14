/* Test the attribute counted_by for pointer field in anonymous struct/union
   and its usage in __builtin_dynamic_object_size.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

struct A {
  int a;
  char *b;
};
struct B {
  float a;
  double b;
};
#define PTR_TYPE struct A
#define FAM_TYPE struct B 
#include "counted-by-anonymous-2.c"
