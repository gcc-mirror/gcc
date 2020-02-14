/* PR rtl-optimization/93088 */
/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops -fno-tree-dominator-opts -fno-tree-vrp -w" } */

#include "pr56348.c"
