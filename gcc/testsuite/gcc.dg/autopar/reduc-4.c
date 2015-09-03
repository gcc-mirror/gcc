/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details -fdump-tree-optimized --param parloops-chunk-size=100" } */

#include "reduc-3.c"
