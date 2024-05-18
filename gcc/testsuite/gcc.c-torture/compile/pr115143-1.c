/* PR tree-optimization/115143 */
/* This used to ICE.
   minmax part of phiopt would transform,
   would transform `a!=0?min(a, b) : 0` into `min(a,b)`
   which was correct except b was defined by a phi in the inner
   bb which was not handled. */
short a, d;
char b;
long c;
unsigned long e, f;
void g(unsigned long h) {
  if (c ? e : b)
    if (e)
      if (d) {
        a = f ? ({
          unsigned long i = d ? f : 0, j = e ? h : 0;
          i < j ? i : j;
        }) : 0;
      }
}

