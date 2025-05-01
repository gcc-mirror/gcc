/* PR c/119317 - Named loops (C2y) did not compile with -O1 and -ggdb2 or higher */
/* { dg-do compile } */
/* { dg-options "-std=c2y -O1 -ggdb2" } */

#include "c2y-named-loops-1.c"
