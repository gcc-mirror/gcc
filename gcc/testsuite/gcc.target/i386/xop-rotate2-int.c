/* PR target/49411 */
/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O2 -mxop" } */

#define NON_CONST 1
#include "xop-rotate1-int.c"
