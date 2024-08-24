// { dg-additional-options "-fschedule-insns -fno-thread-jumps -fno-dce" }
/* { dg-require-effective-target scheduling } */

int a, b, c;
volatile int d;
int e(int f, int g) { return g > 1 ? 1 : f >> g; }
int main() {
  int *i = &a;
  long j[1];
  if (a)
    while (1) {
      a ^= 1;
      if (*i)
        while (1)
          ;
      b = c && e((d, 1) >= 1, j[0]);
    }
  return 0;
}
