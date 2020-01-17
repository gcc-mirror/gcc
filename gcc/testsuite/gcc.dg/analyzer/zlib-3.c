/* { dg-additional-options "-O3 -Wno-analyzer-too-complex" } */
/* TODO: reduce this; was triggering this assert:
            gcc_assert (pruned_state != existing_state);
*/

typedef unsigned char Byte;
typedef unsigned int uInt;

typedef void *voidp;

typedef voidp (*alloc_func)(voidp opaque, uInt items, uInt size);

typedef struct z_stream_s {
  alloc_func zalloc;
  voidp opaque;
} z_stream;

typedef z_stream *z_streamp;

typedef struct inflate_huft_s inflate_huft;

struct inflate_huft_s {
  struct {
    Byte Exop;
    Byte Bits;
  } what;
  uInt base;
};

static int huft_build(uInt *, uInt, uInt, const uInt *, const uInt *,
                      inflate_huft **, uInt *, inflate_huft *, uInt *, uInt *);

static int huft_build(uInt *b, uInt n, uInt s, const uInt *d, const uInt *e,
                      inflate_huft **t, uInt *m, inflate_huft *hp, uInt *hn,
                      uInt *v) {

  uInt a;
  uInt c[15 + 1];
  uInt f;
  int g;
  int h;
  register uInt i;
  register uInt j;
  register int k;
  int l;
  uInt mask;
  register uInt *p;
  inflate_huft *q;
  struct inflate_huft_s r;
  inflate_huft *u[15];
  register int w;
  uInt x[15 + 1];
  uInt *xp;
  int y;
  uInt z;

  p = c;

  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  *p++ = 0;
  p = b;
  i = n;
  do {
    c[*p++]++;
  } while (--i);
  if (c[0] == n) {
    *t = (inflate_huft *)0;
    *m = 0;
    return 0;
  }

  l = *m;
  for (j = 1; j <= 15; j++)
    if (c[j])
      break;
  k = j;
  if ((uInt)l < j)
    l = j;
  for (i = 15; i; i--)
    if (c[i])
      break;
  g = i;
  if ((uInt)l > i)
    l = i;
  *m = l;

  for (y = 1 << j; j < i; j++, y <<= 1)
    if ((y -= c[j]) < 0)
      return (-3);
  if ((y -= c[i]) < 0)
    return (-3);
  c[i] += y;

  x[1] = j = 0;
  p = c + 1;
  xp = x + 2;
  while (--i) {
    *xp++ = (j += *p++);
  }

  p = b;
  i = 0;
  do {
    if ((j = *p++) != 0)
      v[x[j]++] = i;
  } while (++i < n);
  n = x[g];

  x[0] = i = 0;
  p = v;
  h = -1;
  w = -l;
  u[0] = (inflate_huft *)0;
  q = (inflate_huft *)0;
  z = 0;

  for (; k <= g; k++) {
    a = c[k];
    while (a--) {

      while (k > w + l) {
        h++;
        w += l;

        z = g - w;
        z = z > (uInt)l ? l : z;
        if ((f = 1 << (j = k - w)) > a + 1) {
          f -= a + 1;
          xp = c + k;
          if (j < z)
            while (++j < z) {
              if ((f <<= 1) <= *++xp)
                break;
              f -= *xp;
            }
        }
        z = 1 << j;

        if (*hn + z > 1440)
          return (-4);
        u[h] = q = hp + *hn;
        *hn += z;

        if (h) {
          x[h] = i;
          r.what.Bits = (Byte)l;
          r.what.Exop = (Byte)j;
          j = i >> (w - l);
          r.base = (uInt)(q - u[h - 1] - j);
          u[h - 1][j] = r;
        } else
          *t = q;
      }

      r.what.Bits = (Byte)(k - w);
      if (p >= v + n)
        r.what.Exop = 128 + 64;
      else if (*p < s) {
        r.what.Exop = (Byte)(*p < 256 ? 0 : 32 + 64);
        r.base = *p++;
      } else {
        r.what.Exop = (Byte)(e[*p - s] + 16 + 64);
        r.base = d[*p++ - s];
      }

      f = 1 << (k - w);
      for (j = i >> w; j < z; j += f)
        q[j] = r;

      mask = (1 << w) - 1;
      while ((i & mask) != x[h]) {
        h--;
        w -= l;
        mask = (1 << w) - 1;
      }
    }
  }

  return y != 0 && g != 1 ? (-5) : 0;
}

extern const uInt cplens[31];
extern const uInt cplext[31];
extern const uInt cpdist[30];
extern const uInt cpdext[30];

int inflate_trees_dynamic(uInt nl, uInt nd, uInt *c, uInt *bl, uInt *bd,
                          inflate_huft **tl, inflate_huft **td,
                          inflate_huft *hp, z_streamp z) {
  int r;
  uInt hn = 0;
  uInt *v;

  if ((v = (uInt *)(*((z)->zalloc))((z)->opaque, (288), (sizeof(uInt)))) == 0)
    return (-4);

  r = huft_build(c, nl, 257, cplens, cplext, tl, bl, hp, &hn, v);
  r = huft_build(c + nl, nd, 0, cpdist, cpdext, td, bd, hp, &hn, v);
  return 0;
}
