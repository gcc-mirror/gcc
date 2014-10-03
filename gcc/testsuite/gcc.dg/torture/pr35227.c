/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

int
mandel(double _Complex C)
{
  int py;
  C = (__extension__ 1.0iF) * (double)py;
  return cabs(C);  /* { dg-warning "incompatible" } */
}

