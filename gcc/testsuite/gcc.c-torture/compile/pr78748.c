/* PR target/78748 */
/* { dg-options "-march=zEC12" { target { s390*-*-* } } } */

void
foo (int *p, int *q)
{
  *q = *p & ~*q;
}

void
bar (int *p, int *q)
{
  *q = ~*p & *q;
}
