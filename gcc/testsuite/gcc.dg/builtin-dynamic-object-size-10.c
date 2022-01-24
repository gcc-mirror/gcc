/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-early_objsz-details" } */
// { dg-skip-if "packed attribute missing for drone_source_packet" { "epiphany-*-*" } }

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-10.c"

/* early_objsz should resolve __builtin_dynamic_object_size like
   __builtin_object_size.  */
/* { dg-final { scan-tree-dump "maximum object size 21" "early_objsz" } } */
/* { dg-final { scan-tree-dump "maximum subobject size 16" "early_objsz" } } */
