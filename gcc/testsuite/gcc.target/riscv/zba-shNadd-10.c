/* { dg-do run { target { rv64 } } } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64d -O2" } */

struct {
  unsigned a : 14;
  unsigned b : 3;
} c;

unsigned long long d;
void e (unsigned long long *f, long p2) { *f = p2; }
signed g;
long i;

int main () {
  c.b = 4;
  i = -(-c.a - (3023282U + c.a + g));
  e (&d, i);
  if (d != 3023282)
    __builtin_abort ();
  __builtin_exit (0);
}
