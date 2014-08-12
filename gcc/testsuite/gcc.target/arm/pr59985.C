/* { dg-do compile } */
/* { dg-skip-if "incompatible options" { arm_thumb1 } { "*" } { "" } } */
/* { dg-options "-g -fcompare-debug -O2 -march=armv7-a -mtune=cortex-a9 -mfpu=vfpv3-d16 -mfloat-abi=hard" } */

extern void *f1 (unsigned long, unsigned long);
extern const struct line_map *f2 (void *, int, unsigned int, const char *, unsigned int);
extern unsigned int f3 (void *, unsigned int);
extern void *v1;
struct B { const char *s; int t; };
struct C { unsigned u; unsigned long long v; void *w; };
unsigned long long f4 (struct C *);
const char *f5 (void *, unsigned int, unsigned int *);
unsigned long long f6 (void *);

static inline unsigned long long
f7 (struct C *x, unsigned y)
{
  unsigned long long a, b;
  int u = x->u;
  a = y == 64 ? -1ULL : (1ULL << y) - 1;
  if (u + y > 64)
    {
      f6 (x->w);
      x->u = y;
      return b & a;
    }
  b = x->v;
  b >>= u;
  x->u = u + y;
  return b & a;
}

static const char *
f8 (const char *x)
{
  B **a;
  unsigned long t = __builtin_strlen (x);
  char *b;
  struct B *c;
  b = (char *) f1 (t + 1, 1);
  c = (struct B *) f1 (1, sizeof (struct B));
  __builtin_memcpy (b, x, t + 1);
  c->t = t;
  struct B *d = *a;
  return d->s;
}

unsigned int
f9 (struct C *x, void *y)
{
  static const char *a;
  static int b;
  static int c;
  bool d, e, f;
  unsigned t;
  bool prev_file = a != __null;
  if (f7 (x, 1))
    return ((unsigned int) 0);
  d = f7 (x, 1);
  e = f7 (x, 1);
  f = f7 (x, 1);
  a = f8 (f5 (y, f4 (x), &t));
  if (e) b = f4 (x);
  if (f)
    if (d)
      if (prev_file)
        f2 (v1, 1, false, __null, 0);
  return f3 (v1, c);
}
