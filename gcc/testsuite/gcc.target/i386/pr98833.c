/* PR target/98833  */
/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2 -mxop" } */

int __attribute__((__vector_size__(4 * sizeof(int)))) * f5_p;
int __attribute__((__vector_size__(4 * sizeof(int)))) * f6_p;

void f5() { *f5_p = 0 == *f5_p; }
void f6() { *f5_p = *f6_p > *f5_p; }
