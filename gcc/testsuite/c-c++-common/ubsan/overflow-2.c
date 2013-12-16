/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

#define ASM1(a) asm volatile ("" : "+g" (a))
#define ASM2(a, b) asm volatile ("" : "+g" (a), "+g" (b))

#include "overflow-1.c"
