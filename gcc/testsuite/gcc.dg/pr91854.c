/* PR target/91854 */
/* Testcase by Sergei Trofimovich <slyfox@inbox.ru> */
/* { dg-do assemble } */
/* { dg-options "-O2 -Wno-int-conversion" }  */
/* { dg-additional-options "-fPIE -mcpu=niagara4" { target sparc*-*-* } } */

typedef struct {
  long a;
} __attribute__((packed)) c;
void *e, *f;
int i, j, ab, k, l, m, o;
inline int g(int p) {
  if (__builtin_expect(p, 1)) {
    const int aa = ((c *)e)->a ^ ((c *)f)->a;
    if (aa)
      f = sizeof(long);
    return f;
  }
}
void d();
int am ();
inline int n(char p, int u) {
  int q, r, ac = i;
  short b = m;
  while (r && u) {
    if (l) {
      if (k) {
        void *h = i;
        if (__builtin_expect(p, 1)) {
          const int aa = ((c *)e)->a ^ ((c *)h)->a;
          if (aa)
            h = sizeof(long);
          q = h;
        }
      }
      ab = q;
      char s;
      if (s) {
        char t = l = g(t);
      }
    }
    if (j && b)
      if (ac)
        d();
  }
}
void v() {
  const int al = am();
  if (al)
    n(am, v);
an:
  n(am, v);
  if (o)
    goto an;
}
