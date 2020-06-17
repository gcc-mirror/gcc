/* { dg-do compile } */
/* { dg-options "-Wconversion" } */
#include <limits.h>
void foo(char c, char c2)
{
  c >>= c2;
  c >>= 1;
  c <<= 1;
  c <<= c2;
  c += 1;
  c += c2;
  c -= 1;
  c -= c2;
  c *= 2;
  c *= c2;
  c /= 2;
  c /= c2;
  c %= 2;
  c %= c2;
  c = -c2;
  c = ~c2;
  c = c2++;
  c = ++c2;
  c = c2--;
  c = --c2;
}

void bar(char c, int c2)
{
  c >>= c2; 
  c >>= (int)1;
  c <<= (int)1;
  c <<= c2;
  c += ((int)CHAR_MAX + CHAR_MAX); /* { dg-warning "conversion" } */
  c += c2; /* { dg-warning "conversion" } */
  c -= ((int)CHAR_MAX + CHAR_MAX); /* { dg-warning "conversion" } */
  c -= c2; /* { dg-warning "conversion" } */
  c *= ((int)CHAR_MAX + CHAR_MAX); /* { dg-warning "conversion" } */
  c *= c2; /* { dg-warning "conversion" } */
  c /= ((int)CHAR_MAX + CHAR_MAX); /* { dg-warning "conversion" } */
  c /= c2; /* { dg-warning "conversion" } */
  c %= ((int)CHAR_MAX + CHAR_MAX); /* { dg-warning "conversion" } */
  c %= c2; /* { dg-warning "conversion" } */
  c = ~c2; /* { dg-warning "conversion" } */
  c = c2++; /* { dg-warning "conversion" } */
  c = ++c2; /* { dg-warning "conversion" } */
  c = c2--; /* { dg-warning "conversion" } */
  c = --c2; /* { dg-warning "conversion" } */
}
