/* Test for _Atomic: not in C90.  */
/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic-errors" } */

_Atomic int i; /* { dg-error "_Atomic" } */
_Atomic (int) j; /* { dg-error "_Atomic" } */
int *_Atomic p; /* { dg-error "_Atomic" } */
