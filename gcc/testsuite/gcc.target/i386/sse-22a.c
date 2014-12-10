/* { dg-do compile } */
/* { dg-options "-O0 -Werror-implicit-function-declaration -march=k8" } */
/* { dg-add-options bind_pic_locally } */

#define DIFFERENT_PRAGMAS

#include "sse-22.c"
