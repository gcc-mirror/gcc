/* Test the attribute counted_by for pointer field in anonymous struct/union
   and its usage in __builtin_dynamic_object_size.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

#define PTR_TYPE char 
#define FAM_TYPE char 
#include "counted-by-anonymous-2.c"
