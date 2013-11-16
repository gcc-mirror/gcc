/* Test for _Atomic: not in C99.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

_Atomic int i; /* { dg-error "_Atomic" } */
_Atomic (int) j; /* { dg-error "_Atomic" } */
int *_Atomic p; /* { dg-error "_Atomic" } */
void f (int a[_Atomic]); /* { dg-error "_Atomic" } */
