/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

int a[_Maxof(char)];  /* { dg-warning "ISO C does not support" } */
int b[1 + _Minof(unsigned char)];  /* { dg-warning "ISO C does not support" } */
