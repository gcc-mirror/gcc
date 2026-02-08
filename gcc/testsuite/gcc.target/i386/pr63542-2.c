/* PR target/63542 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -dA" } */
/* { dg-additional-options "-fpic" { target fpic } } */

struct B { __UINTPTR_TYPE__ c; unsigned char *d; };
extern struct A { struct B *e[0x400]; } *f[128];
extern void (*bar) (char *p, char *q);

char *
foo (char *p, char *q)
{
  struct B *g;
  char *b, *l;
  __UINTPTR_TYPE__ s;

  g = f[((__UINTPTR_TYPE__) p) >> 22]->e[(((__UINTPTR_TYPE__) p) >> 12) & 0x3ff];
  s = g->c << 2;
  int r = ((__UINTPTR_TYPE__) p) & 0xfff;
  int m = g->d[r];
  if (m > 0xfd)
    {
      m = (r >> 2) % (s >> 2);
      if ((((__UINTPTR_TYPE__) p) & ~(__UINTPTR_TYPE__) 0xfff) != (((__UINTPTR_TYPE__) q) & ~(__UINTPTR_TYPE__) 0xfff))
	goto fail;
    }
  b = (char *) ((__UINTPTR_TYPE__) p & ~(__UINTPTR_TYPE__) 3);
  b -= m << 2;
  l = b + s;

  if ( q >= l || q < b)
    goto fail;
  return p;
fail:
  (*bar) (p, q);
  return p;
}
