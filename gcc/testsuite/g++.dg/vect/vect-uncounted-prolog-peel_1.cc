/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-w" } */

namespace a {
class b {
public:
  b d(long);
};
class e : public b {};
typedef short f;
typedef struct g *h;
typedef struct g {
  f c;
  e *fp;
  f j, *k, *l;
} m;
static h n(m *, unsigned, unsigned *);
f o(m *, unsigned);
void p(short *t) {
  m *ffile;
  int q;
  unsigned r;
  int i;
  q = t[i];
  n(ffile, q, &r);
}
h n(m *ffile, unsigned q, unsigned *) {
  h glyph;
  for (; glyph;)
    if (glyph->c)
      o(ffile, q);
  int i;
s:
  for (i = 0; ffile->j; i++)
    if (ffile->k[i]) {
      if (q)
        ffile->fp->d(ffile->l[i]);
      break;
    }
  if (q < 6) {
    q += 61440;
    goto s;
  }
}
} // namespace a

/* { dg-final { scan-tree-dump {note:\s*Alignment of access forced using peeling.} "vect" } } */
/* { dg-final { scan-tree-dump {vectorized 1 loops in function} "vect" } } */
