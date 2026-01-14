/* Test the attribute counted_by for pointer field in anonymous struct/union
   and its usage in __builtin_dynamic_object_size.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

#define PTR_TYPE float 
#define FAM_TYPE float 
#include "counted-by-anonymous-2.c"
