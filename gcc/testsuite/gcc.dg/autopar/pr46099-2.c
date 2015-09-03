/* PR tree-optimization/46099.  */
/* { dg-do compile } */
/* { dg-options "-ftree-parallelize-loops=2 -fcompare-debug -O --param parloops-chunk-size=100" } */

#include "pr46099.c"
