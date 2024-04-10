/* { dg-do compile } */
/* { dg-options { -O3 -fno-vect-cost-model -march=armv9-a -msve-vector-bits=256 } } */

unsigned a;
int b;
long *c;

int
main ()
{
  for (int d = 0; d < 22; d += 4) {
      b = ({
           int e = c[d];
           e;
           })
      ? 0 : -c[d];
      a *= 3;
  }
}
