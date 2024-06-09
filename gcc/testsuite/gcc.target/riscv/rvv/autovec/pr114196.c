/* { dg-do compile } */
/* { dg-options { -O3 -fno-vect-cost-model -march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl } } */

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
