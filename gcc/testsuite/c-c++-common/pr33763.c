/* PR tree-optimization/33763 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef struct
{
  void *a;
  void *b;
} T;
extern void *foo (const char *, const char *);
extern void *bar (void *, const char *, T);
extern int baz (const char *, int);

extern inline __attribute__ ((always_inline, gnu_inline)) int
baz (const char *x, int y)
{
  return 2;
}

int
baz (const char *x, int y)
{
  return 1;
}

int xa, xb;

static void *
inl (const char *x, const char *y)
{
  T t = { &xa, &xb };
  int *f = (int *) __builtin_malloc (sizeof (int));
  const char *z;
  int o = 0;
  void *r = 0;

  for (z = y; *z; z++)
    {
      if (*z == 'r')
	o |= 1;
      if (*z == 'w')
	o |= 2;
    }
  if (o == 1)
    *f = baz (x, 0);
  if (o == 2)
    *f = baz (x, 1);
  if (o == 3)
    *f = baz (x, 2);

  if (o && *f > 0)
    r = bar (f, "w", t);
  return r;
}

void *
foo (const char *x, const char *y)
{
  return inl (x, y);
}
