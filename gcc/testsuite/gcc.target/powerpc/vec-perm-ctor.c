/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx -fdump-tree-optimized" } */

/* To test all permutations fed by CTOR and CST can be optimized away.  */

#include "vec-perm-ctor.h"

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "optimized" } } */
