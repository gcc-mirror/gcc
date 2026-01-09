/* { dg-do compile } */
/* { dg-options "-std=c23" } */

const enum E { A } *x;

/* qualifier mismatch */
_Static_assert(_Generic(x, unsigned int *: 0, default: 1), "");

