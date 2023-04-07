/* { dg-do compile { target { lp64 || llp64 } } } */
struct A { int b[1]; };

void
foo (struct A *d)
{
  d->b[0] = d->b[-144115188075855873LL] + d->b[11] * d->b[2]
          + d->b[0] % d->b[1025] + d->b[5];
  d->b[0] = d->b[144678138029277184LL] + d->b[0] & d->b[-3] * d->b[053]
          + d->b[7] ^ d->b[-9] + d->b[14] + d->b[9] % d->b[49]
          + d->b[024] + d->b[82] & d->b[4096];
}
