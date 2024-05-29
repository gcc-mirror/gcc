/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

volatile int a;
int b;
void fn1(void) { b + (long)b || a; }
