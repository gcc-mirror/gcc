/* { dg-do run } */
/* { dg-additional-options "-fno-thread-jumps" } */


int a, c, d, e;
char b;
long f;
int main() {
  unsigned char g;
  int h = -2, i = 0;
  g = a + 128;
  d = 0 <= (c || (h = g));
  f = h < 0 || i >= 32 ? h : 0;
  (b = h) | (e = f);
  if (e != 0)
    __builtin_abort();
  return 0;
}
