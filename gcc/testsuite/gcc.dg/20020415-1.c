/* PR target/6303
   This testcase ICEd because s390 did not define
   ASM_SIMPLIFY_DWARF_ADDR hook.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fpic -g" } */
/* { dg-warning "not supported" "PIC unsupported" { target cris-*-elf* mmix-*-* } 0 } */

static inline char *
bar (unsigned long x, char *y)
{
  extern const char ext[];
  const char *a = ext;
  char *b = y;

  do *--b = a[x % 10]; while ((x /= 10) != 0);
  return b;
}

struct A { char *p, *q; };
struct B { int r, s; };

int
foo (struct A *a, const struct B *b)
{
  char c[(b->r > b->s) ? b->r : b->s];
  char *d = &c[sizeof c];
  register char *e;

  e = bar (b->r, d);
  while (e < d)
    {
      register const int f = *e++;
      if (((a->p >= a->q) ? 1 : (unsigned char) (*a->p++ = f)) == -1)
	break;
    }
}
