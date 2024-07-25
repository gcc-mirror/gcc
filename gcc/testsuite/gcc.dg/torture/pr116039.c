/* { dg-do run } */
/* { dg-additional-options "-fsigned-char -fno-strict-aliasing -fwrapv" } */

extern void abort (void);

int c[12];
char d[12];
int *f = c;
int *z = (int *)1;
long long y;
int main() {
  c[9] = 0xff;
  for (int i = 0; i < 12; i += 3)
    d[9] = z ? f[i] : 0;
  for (long i = 0; i < 12; ++i)
    y ^= d[i];
  if (y != -1)
    abort ();
  return 0;
}
