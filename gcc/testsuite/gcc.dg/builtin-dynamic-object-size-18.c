/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* __stpncpy_chk could return buf up to buf + 64, so
   the minimum object size might be far smaller than 64.  */
/* { dg-final { scan-tree-dump-not "return 64;" "optimized" } } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-18.c"
