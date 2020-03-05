/* PR target/91269 */
/* Testcase by Sergei Trofimovich <slyfox@inbox.ru> */

/* { dg-do assemble } */
/* { dg-options "-O2 -Wno-int-conversion" }  */
/* { dg-additional-options "-fcall-used-g6 -fPIE -mcpu=niagara4" { target sparc*-*-* } } */

struct m;

enum { a = 2 };
int b[1];
int d[2715];
int e, f, h;
enum { i = 2 } j;
inline int c(int k) {
  char *cp;
  if (k >= 62 && k <= 247)
    cp = b[k];
  if (cp)
    return 65533;
  return 2;
}
inline int g(int k) {
  if (k < sizeof(d))
    return e;
  return 0;
}

int u(struct m*, char*, char*);

int l(struct m *k, char n, long o, int *p) {
  int q, flags = j, r, s, lasttwo = *p;
  char inptr, outptr;
  while (inptr) {
    if (__builtin_expect(h, 0))
      break;
    unsigned ch = inptr;
    if (lasttwo) {
      long need = lasttwo >> 3;
      if (__builtin_expect(need > n, 0))
        break;
    } else if (s == i) {
      long t = c(ch);
      if (t != 65533) {
        int jch = g(ch);
        if (jch & 8)
          continue;
      }
    }
    if (ch <= 5)
      ;
    else {
      long t = c(ch);
      if (t != 65533)
        ;
      else {
        switch (f >> 8)
        case 79:
          q = f == 20308 || f == 20350;
        if (q)
          if (j)
            r = u(k, &inptr, &outptr);
        s = *p;
        if (r)
          if (o && flags & a)
            break;
      }
    }
  }
}
