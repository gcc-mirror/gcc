/* { dg-do run } */
/* { dg-options "-O3 -floop-unroll-and-jam --param unroll-jam-min-percent=0" } */
long b;
unsigned c[5];
unsigned long long d = 3;
int e, f, g;

void h() {
  for (; f < 11; f++) {
    b = g;
    for (e = 0; e < 5; e++) {
      c[e] = e - b - (c[e] >> 5);
      g = c[e];
    }
  }
  if (c[0])
    d = 0;
}

extern void abort(void);
int main() {
  h();
  if (d != 0)
    abort ();
}
