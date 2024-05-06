/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl128b -mabi=lp64d -fwhole-program -O3 -mrvv-vector-bits=zvl" } */

extern int a[];
extern char b[];
int c = 24;
_Bool d[24][24][24];
_Bool (*e)[24][24] = d;
int main() {
  for (short f = 0; f < 24; f += 3)
    for (unsigned g = 0; g < (char)c; g += 2) {
      a[f] = 0;
      b[g] |= ({ e[f][f][f]; });
    }
}
