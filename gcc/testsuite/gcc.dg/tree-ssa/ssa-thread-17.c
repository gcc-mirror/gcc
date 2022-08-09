/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-threadfull1-details --param max-jump-thread-paths=15" } */

#include "ssa-thread-16.c"

/* With limiting the search space we should no longer consider this path.  */
/* { dg-final { scan-tree-dump-not "SUCCESS" "threadfull1" } } */
