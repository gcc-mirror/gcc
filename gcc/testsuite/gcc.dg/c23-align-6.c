/* Test C23 alignof returning minimum alignment for a type.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#define _Alignas alignas
#define _Alignof alignof

#include "c11-align-6.c"
