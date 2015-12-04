/* { dg-options "-floop-nest-optimize -O2" } */

typedef struct {
  unsigned lp, lc;
  short *l;
  short p[1 << 4];
} foo;

void LzmaEnc_Init(foo *p) {
  unsigned i;
  unsigned num = 0x300 << (p->lp + p->lc);
  for (i = 0; i < num; i++)
    p->l[i] = ((1 << 11) >> 1);

  for (i = 0; i < (1 << 4); i++)
    p->p[i] = ((1 << 11) >> 1);
}
