/* { dg-do compile { target fpic } } */
/* { dg-options "-fPIC -g -O2 -w" } */
/* var-tracking failed to clobber the reg holding v at the asm insn,
   so v ended up bound to an intermediate PIC expression.  */

struct A { unsigned a1; char a2[15]; };
struct B { long b1; unsigned char b2; long b3; };
struct C { void *c1; unsigned c2; unsigned c3; };

static struct A v1;
struct A *const v2 = &v1;

static inline
int foo (void)
{
  int *v;
  __asm__ __volatile__ ("" : "=r" (v));
  return v[1];
}

static void
bar (struct C *x)
{
  if (x->c2 == x->c3 && x->c1)
    f1 (foo (), x->c1, x->c3 * sizeof (x->c1[0]));
}

void
baz (struct B *y)
{
  int i;
  const char *j;
  char *k;
  char x[64];
  for (i = 0; i < sizeof (struct B); i++, y)
    {
      switch (y->b2)
        {
        case 0x20:
          if (__builtin_strchr (j, '='))
            continue;
        }
      switch (y->b2)
        {
        case 0x80:
          bar (&x);
          f2 (y->b3);
        case 0x2e:
        case 0x4e:
          break;
        default:
          if (v2->a1)
            f2 (y->b2);
        }
      k[0] = '\0';
      if (v2->a1)
        f2 (y->b1);
    }
}
