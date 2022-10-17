/* { dg-do compile } */
/* { dg-require-effective-target fixed_point } */
/* { dg-options "-fnon-call-exceptions" } */

_Accum sa;
int c;

void div_csa() { c /= sa; }
