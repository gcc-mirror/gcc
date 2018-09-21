/* PR tree-optimization/86990 */
/* Testcase by Zhendong Su <su@cs.ucdavis.edu> */

const char *ss;

int __attribute__((noipa)) dummy (const char *s, ...)
{
  ss = s;
}

int i[6];
static int j, v, e, f, h = 5, k, l, n, o, p, q, r, s, u, w, x, y, z, aa, ab, ac,
                       ad, ae, af, ag = 8, ah, ai, aj, ak, al;
char c;
struct a {
  unsigned b;
  int c : 9;
  int d;
} static g = {9, 5};
static short m[1], t = 95, am;
int an, ao, ap;
void aq(int ar) {
  j = j & 5 ^ i[j ^ v & 5];
  j = j & 5 ^ i[(j ^ v) & 5];
  j = j & 4095 ^ (j ^ v) & 5;
}
void as(int ar) {
  if (n)
    s = 0;
}
static unsigned at() {
  int au[] = {2080555007, 0};
  for (; al; al--) {
    if (r)
      --x;
    if (g.d)
      l++;
    dummy("", j);
    if (u)
      ae = n = au[al];
  }
  r = 0;
  return 0;
}
int aw(int ar) {
  int ax[] = {9, 5, 5, 9, 5}, ay = 3;
  struct a az = {1, 3};
av:
  an = (as((at(), ax)[2]), ax[4]);
  {
    int ba[] = {5, 5, 9, 8, 1, 0, 5, 5, 9, 8, 1, 0,
                5, 5, 9, 8, 1, 0, 5, 5, 9, 8, 1};
    int a[] = {8, 2, 8, 2, 8, 2, 8};
    int b[] = {1027239, 8, 1, 7, 9, 2, 9, 4, 4, 2, 8, 1, 0, 4, 4, 2,
               4,       4, 2, 9, 2, 9, 8, 1, 7, 9, 2, 9, 4, 4, 2};
    if (z) {
      struct a bc;
    bb:
      for (; e; e++)
        for (; q;)
          return ax[e];
      if (bc.c < g.d <= a[7])
        aa--;
    }
    {
      struct a bd = {5};
      int d[20] = {1, 9, 7, 7, 8, 4, 4, 4, 4, 8, 1, 9, 7, 7, 8, 4, 4, 4, 4};
      c = h | r % g.c ^ x;
      dummy("", g);
      am -= t | x;
      if (h)
        while (1) {
          if (a[o]) {
            struct a be;
            if (ar) {
              struct a bf = {908, 5, 3};
              int bg[3], bh = k, bj = ag | ae, bk = aj + 3, bl = u << e;
              if (f)
                if (ac)
                  ak = w;
              ag = -(ag & t);
              af = ag ^ af;
              if (8 < af)
                break;
              if (bj)
                goto bi;
              if (s)
                dummy("", 6);
              be.d = k;
              w = f - bh;
              dummy("", be);
              if (w)
                goto bb;
              ao = r - aa && g.b;
              if (y)
                k++;
              goto av;
            bi:
              if (aa)
                continue;
              if (f)
                if (k)
                  dummy("", g);
              aj = ac + k ^ g.c;
              g.c = bk;
              ah = 0;
              for (; ah < 3; ah++)
                if (s)
                  bg[ah] = 8;
              if (!ay)
                dummy("", ai);
              u = bl;
              g = bf;
            } else
              for (;; o += a[ap])
                ;
            int bm[] = {0};
            for (; p; p++)
              c = ad;
            ad = l;
            if (bd.c) {
              dummy(" ");
              goto bi;
            }
          }
          int bn[] = {5, 2, 2, 5, 2, 2, 5, 2, 2, 5, 2, 2, 5, 2, 2, 5,
                      2, 2, 5, 2, 2, 5, 2, 2, 5, 2, 2, 5, 2, 2, 5, 2,
                      2, 5, 2, 2, 5, 2, 2, 5, 2, 2, 5, 2, 2, 5, 2};
          struct a a[] = {3440025416, 2, 8, 4, 2, 8, 4, 4, 2, 8, 4};
          struct a b = {3075920};
          if (f) {
            aq(m[am + e]);
            dummy("", j);
            dummy("", e);
            ab--;
          }
          if (ax[4]) {
            if (l)
              goto av;
            ++f;
          } else
            ay = az.c && a;
          for (; ac; ac++)
            m[f] = 0;
        }
      h = 9;
      for (; y; y = 1)
        if (f)
          goto av;
    }
  }
  return 0;
}

int main (void)
{
  aw(1);
  if (g.c!= 5)
    __builtin_abort ();
  return 0;
}
