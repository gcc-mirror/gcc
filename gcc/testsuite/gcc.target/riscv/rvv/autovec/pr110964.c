/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -Ofast" } */

int *a;
long b, c;

int d ()
{
  const int e;
  for (; a < e; a++) /* { dg-warning "comparison between pointer and integer" } */
    c += *a * b;
}

