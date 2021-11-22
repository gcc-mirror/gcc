/* { dg-do run { target { s390_useable_hw } } } */
/* { dg-options "-Os -march=z10" } */
signed char a;
int b = -925974181, c;
unsigned *d = &b;
int *e = &c;
int main() {
  *e = ((217 ^ a) > 585) < *d;
  if (c != 1)
    __builtin_abort();
  return 0;
}
