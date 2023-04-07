/* { dg-do run { target aarch64_sve512_hw } } */
/* { dg-options "-O3 -msve-vector-bits=512" } */

long d = 1;
static int i = 37;
static unsigned long a[22];
static unsigned short c[22];
static unsigned g[80];
static unsigned short *h = c;
static unsigned long *j = a;
int main() {
  for (long m = 0; m < 8; ++m)
    d = 1;
  for (unsigned char p = 0; p < 17; p += 2)
  {
    long t = h[p] ? i : j[p];
    g[p] = t;
  }
  if (g[0])
    __builtin_abort ();
}
