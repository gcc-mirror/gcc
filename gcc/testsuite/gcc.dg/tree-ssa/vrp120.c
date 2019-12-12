/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */
#include "vrp113.c"

/* { dg-final { scan-tree-dump "return 3;" "evrp" } } */
