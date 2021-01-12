/* { dg-options "-O3 -msve-vector-bits=512" } */

long c;
int a;
int e[14];
short b[14];
void d(long *f, long h) { *f ^= h + *f; }
void this_test_has_completed_successfully ();
int main() {
  e[2] = 1;
  for (int g = 0; g < 13; g++)
    a = b[g] = e[g];
  d(&c, a);
  for (int g = 0; g < 4; g++)
    d(&c, b[2]);
  if (c != 15)
    __builtin_abort();
  this_test_has_completed_successfully ();
}

/* { dg-final { scan-assembler {this_test_has_completed_successfully} } } */
