/* PR target/63542 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -dA" } */
/* { dg-additional-options "-fpic" { target fpic } } */

struct B { unsigned long c; unsigned char *d; };
extern struct A { struct B *e[0x400]; } *f[128];
extern void (*bar) (char *p, char *q);

char *
foo (char *p, char *q)
{
  struct B *g;
  char *b, *l;
  unsigned long s;

  g = f[((unsigned long) p) >> 22]->e[(((unsigned long) p) >> 12) & 0x3ff];
  s = g->c << 2;
  int r = ((unsigned long) p) & 0xfff;
  int m = g->d[r];
  if (m > 0xfd)
    {
      m = (r >> 2) % (s >> 2);
      if ((((unsigned long) p) & ~(unsigned long) 0xfff) != (((unsigned long) q) & ~(unsigned long) 0xfff))
	goto fail;
    }
  b = (char *) ((unsigned long) p & ~(unsigned long) 3);
  b -= m << 2;
  l = b + s;

  if ( q >= l || q < b)
    goto fail;
  return p;
fail:
  (*bar) (p, q);
  return p;
}
