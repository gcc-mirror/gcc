/* { dg-do run { target bitint } } */
/* { dg-options "-O3" } */

int g, x;
_BitInt(3) n;
int main(void) {
  for (; n; n++) {
    short l = g;
    for (int i = 0; i < 14 && l > (g > 0); i++) {
      _BitInt(24) r;
      l = (__builtin_add_overflow(l, 1, &r) ?: r) - l;
    }
    x = l;
  }
}
