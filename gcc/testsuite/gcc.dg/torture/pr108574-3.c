/* { dg-do run } */

__INT32_TYPE__ a = 3557301289, d;
signed char b, f;
unsigned short c = 241;
short e, g;
static void h() {
  if (!a)
    goto i;
  b = a;
  for (; a < 2; a = b) {
    unsigned short j;
    if (c || !g) {
      j = c;
    i:
      e = j;
    }
    f = j;
    d = ~(f & ~2880764155);
    while (d > -2316069)
      ;
  }
}
int main() {
  h();
  return 0;
}
