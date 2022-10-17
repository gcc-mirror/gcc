/* { dg-do compile } */
/* { dg-additional-options "-floop-unroll-and-jam" } */

char a, b;
extern unsigned short c[];
extern bool d[];
const unsigned short &e(const unsigned short &f, const unsigned short &g) {
  if (g < f)
    return g;
  return f;
}
void k() {
  for (int h = 0; b; h += 3)
    for (unsigned long i = 0; i < 11104842004558084287ULL;
         i += -11104842004558084300ULL)
      for (bool j(e(6, e(6, c[h + i]))); j < (bool)a; j = 7)
        d[7] = 0;
}
