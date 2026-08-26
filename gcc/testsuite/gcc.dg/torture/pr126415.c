/* PR tree-optimization/126415 */
/* Wrong code from the inverse widening rewrite (T)A +- CST -> (T)(A +- CST'):
   the introduced narrow signed operation may overflow even though the
   original widened operation is fully defined.  The narrow op then collapses
   through defined-wrap identities (mod 2^narrow-prec), changing the value.  */
/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int printf(const char *, ...);

long d(short p1, unsigned e, char f) {
  if (246 >= 149u - f)
    return 0;
  return f;
}
int g(char p1) { return d(0, 0, p1); }
int fn3(char p1) {
  long i = g(p1 + 159);
  return i;
}

/* Minimal variants: signed char/short/int inner, PLUS and MINUS, and a
   negative CST encoded as a large unsigned constant.  Use signed char
   explicitly: the checks encode sign-extension results and plain char
   is unsigned on some targets.  */
volatile signed char vc1 = -84;
volatile signed char vc2 = 50;
volatile short vs = -21000;
volatile int vi = -2000000000;
volatile signed char vc3 = 100;

int main() {
  if (fn3(-84) != 0)
    __builtin_abort ();

  signed char p1 = vc1;
  signed char f1 = (signed char)((unsigned char)p1 + 159);
  if ((unsigned)f1 + 97 != 172u)
    __builtin_abort ();

  signed char p2 = vc2;
  signed char f2 = (signed char)((unsigned char)p2 + 97);
  if ((unsigned)f2 - 97 != 4294967090u)
    __builtin_abort ();

  short p3 = vs;
  short f3 = (short)((unsigned short)p3 + 40000);
  if ((unsigned)f3 + 25536 != 44536u)
    __builtin_abort ();

  int p4 = vi;
  int f4 = (int)((unsigned)p4 + 3000000000u);
  if ((unsigned long long)f4 + 1294967296ull != 2294967296ull)
    __builtin_abort ();

  signed char p5 = vc3;
  signed char f5 = (signed char)((unsigned char)p5 + 97);
  if ((unsigned)f5 + 0xFFFFFF9Fu != 4294967140u)
    __builtin_abort ();

  return 0;
}
