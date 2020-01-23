/* { dg-do compile } */
/* { dg-options "-Wconversion -Warith-conversion" } */
#include <limits.h>
void foo(char c, char c2)
{
  c >>= c2;			/* { dg-warning "conversion" } */
  c >>= 1;
  c <<= 1;			/* { dg-warning "conversion" } */
  c <<= c2;			/* { dg-warning "conversion" } */
  c += 1;			/* { dg-warning "conversion" } */
  c += c2;			/* { dg-warning "conversion" } */
  c -= 1;			/* { dg-warning "conversion" } */
  c -= c2;			/* { dg-warning "conversion" } */
  c *= 2;			/* { dg-warning "conversion" } */
  c *= c2;			/* { dg-warning "conversion" } */
  c /= 2;
  /* If char is unsigned we avoid promoting to int.  */
  c /= c2;  /* { dg-warning "conversion" "" { target { ! unsigned_char } } } */
  c %= 2;
  c %= c2;  /* { dg-warning "conversion" "" { target { ! unsigned_char } } } */
  c = -c2;			/* { dg-warning "conversion" } */
  c = ~c2;			/* { dg-warning "conversion" } */
  c = c2++;
  c = ++c2;
  c = c2--;
  c = --c2;
}

void bar(char c, int c2)
{
  c >>= c2; 			/* { dg-warning "conversion" } */
  c >>= (int)1;
  c <<= (int)1;			/* { dg-warning "conversion" } */
  c <<= c2;			/* { dg-warning "conversion" } */
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
