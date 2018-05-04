/* PR libstdc++/85466 */
/* { dg-do run } */
/* { dg-options "-O2 -fmath-errno -fno-trapping-math -fdump-tree-optimized" } */
/* { dg-add-options ieee } */
/* { dg-final { scan-tree-dump-not "nextafter" "optimized" } } */
/* { dg-final { scan-tree-dump-not "nexttoward" "optimized" } } */

#define NEED_ERRNO 1
#include "nextafter-1.c"
