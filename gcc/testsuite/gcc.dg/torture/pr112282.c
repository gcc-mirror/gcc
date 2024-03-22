/* { dg-do run } */

#if __SIZEOF_INT__ < 4
#define Xint __INT32_TYPE__
#else
#define Xint int
#endif

int printf(const char *, ...);
void abort ();
/* We need an abort that isn't noreturn.  */
void __attribute__((noipa)) my_abort ()
{
  abort ();
}
int a, g, h, i, v, w = 2, x, y, ab, ac, ad, ae, af, ag;
static int f, j, m, n, p, r, u, aa;
struct b {
  Xint c : 20;
  Xint d : 20;
  int e : 10;
};
static struct b l, o, q = {3, 3, 5};
int s(int z) {
  struct b ah;
  int ai = 1, aj[7] = {1, 1, 1, 1, 1, 1, 1};
ak:
  for (u = -22; u < 2; ++u) {
    struct b al[8] = {{2, 7, 9}, {8, 7, 1}, {2, 7, 9}, {8, 7, 1}, {2, 7, 9}, {8, 7, 1}, {2, 7, 9}};
    y = z = 0;
    for (; z < 2; z++) {
      int am[18], k;
      ab = ac = 0;
      for (; ac < 1; ac++)
        for (k = 0; k < 9; k++)
          am[k] = 0;
      n = 0;
      while (1) {
        v = u < 0 || a;
        h = z < ~u && 4 & q.c;
        if ((aa <= l.c) > q.d && p)
          return o.c;
        if (w)
          break;
        return q.e;
      }
      a = j;
    }
  }
  for (x = 0; x < 2; x++) {
    struct b an = {1, 8, 4};
    int ao[28] = {5, 0, 0, 9, 0, 3, 0, 5, 0, 0, 9, 0, 3, 0, 5, 0, 0, 9, 0, 3, 0, 5, 0, 0, 9, 0, 3, 0};
    if (q.e) {
      int ap = ai || l.c + q.c, aq = q.d, ar = p & f;
      q.d = q.d || ar || ap;
      p = 0;
      if (!j && ai)
        goto as;
      if (q.d) {
        printf("", l);
        q.d = f >> j;
      }
      p = l.c = aq;
      an = q;
    } else {
      int at[12][1] = {{9}, {9}, {5}, {9}, {9}, {5}, {9}, {9}, {5}, {9}, {9}, {5}};
      struct b au;
      if (o.c)
        aa = ah.e;
      if (an.d)
        ah.e = (j & (aa * m)) ^ au.d;
      o.c = m + aa;
      int av = o.c || 0, aw = ai || q.c & l.c, ax = n;
      if (q.e < ai)
        q = an;
      if (r)
        break;
      ai = aw - av;
      an.e = 0;
      if (ai) {
        an.e = l.c || 0;
        f = q.c;
        ah.e = l.c % q.d;
        q.c = au.e;
        if ((q.d && q.c) || ah.e)
          my_abort ();
        q.c = 0;
        if (au.d > m || ah.e)
          w = au.c | (n & ah.c);
      as:
        ae = af = ah.c;
        int ay = au.d & q.e & au.c || o.c, az = 0 || o.c, ba = m & ah.d;
        if (n)
          au.c = au.e = (q.e || ah.d) ^ (o.c + (az / au.e));
        n = au.c || au.e;
        if (ba) {
          printf("", ax);
          x = q.e | m;
          continue;
        }
        m = ay;
        n = printf("", au);
      }
      if (ah.d)
        o.c = l.c & o.c & q.c;
      if (q.d)
        my_abort ();
      printf("", an);
      printf("", q);
      printf("", au);
      if (ah.e)
        while (u++) {
          struct b al[7] = {{7, 9, 8}, {7, 1, 2}, {7, 9, 8}, {7, 1, 2}, {7, 9, 8}, {7, 1, 2}, {7, 9, 0}};
          if (an.d) {
            int d[8] = {0, 1, 0, 1, 0, 1, 0, 1};
            if (ad)
              goto ak;
            while (ag)
              g = an.d = i = m;
            f = j;
          }
          n++;
        }
      f = q.d;
    }
    if (l.c && m) {
      int d[7] = {1, 0, 1, 0, 1, 0, 1};
      if (x)
        h = an.d;
      else
        g = 0;
    }
  }
  int bb = (q.d ^ ah.c) | aa | (q.e & q.c) | (f & ah.d);
  if (bb)
    return x;
  return 0;
}
int main() {
  j = 1;
  s(0);
  return 0;
}
