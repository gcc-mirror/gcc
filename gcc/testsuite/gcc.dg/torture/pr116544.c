/* { dg-options "-fno-strict-aliasing -fwrapv" }
/* { dg-do run { target longlong64 } } */

extern void abort (void);
long long a;
signed char b[60];
signed char c;
long long d[60];
int e[30];
long long *f = d;
static void g(long long *j, long k) { *j = k; }
int main() {
  d[5] = 0x100000000;
  for (int h = 2; h < 7; h += 3)
    for (int i = 0; i < (c || b[h]) + 10; i += 11)
      e[2] = f[h];
  g(&a, e[2]);
  if (a != 0)
    abort ();
  return 0;
}

