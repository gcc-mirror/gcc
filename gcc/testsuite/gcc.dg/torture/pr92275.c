/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

unsigned long a, c;
int *b, *b2;
long d;

void fn1()
{
  for (; b < b2; b++)
    d += *b * c;
  d *= a;
}
