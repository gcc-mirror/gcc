/* { dg-do run } */

void abort (void);
int a, c;
long b;
short d;
static long e(long f, long h, long i) {
  for (long g = f; g <= h; g += i)
    b += g;
  return b;
}
int main() {
  c = 1;
  for (; c >= 0; c--)
    ;
  for (; e(d + 40, d + 76, c + 51) < 4;)
    ;
  if (a != 0)
    abort ();
}
