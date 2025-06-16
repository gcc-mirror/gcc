/* Test the attribute counted_by for pointer field and its usage in
 * __builtin_dynamic_object_size.  */ 
/* { dg-do run } */
/* { dg-options "-O2" } */
#define PTR_TYPE char
#include "pointer-counted-by-4.c"
