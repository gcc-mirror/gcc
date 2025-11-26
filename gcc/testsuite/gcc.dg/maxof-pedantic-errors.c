/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a[_Maxof(char)];  /* { dg-error "ISO C does not support" } */
int b[1 + _Minof(unsigned char)];  /* { dg-error "ISO C does not support" } */
