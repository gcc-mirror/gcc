/* PR tree-optimization/80345 */

typedef long mp_limb_signed_t;
void fn1(mp_limb_signed_t p1) {
  int *a = (int *)1;
  mp_limb_signed_t i, j;
  i = 0;
  for (; i < p1; i++) {
    j = 0;
    for (; j <= i; j++)
      *a++ = 0;
    j = i + 1;
    for (; j < p1; j++)
      a++;
  }
}
void fn2() { fn1((mp_limb_signed_t)fn2); }
