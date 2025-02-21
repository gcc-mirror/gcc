/* { dg-do compile }
   { dg-additional-options "-O2 -fdump-tree-optimized" } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-alloc-size.c"
/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
