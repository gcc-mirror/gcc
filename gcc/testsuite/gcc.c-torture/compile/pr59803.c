/* PR target/59803 */

extern void baz (void) __attribute__ ((__noreturn__));
struct A { int g, h; };
extern struct A a;
struct B { unsigned char i, j, k, l, m; };
int c, d, e;
static int f;

void
foo (void)
{
  f = 1;
}

void
bar (struct B *x)
{
  x->i = e;
  x->k = c;
  x->l = d;
  x->j = a.h;
  x->m = f;
  if (x->i != e) baz ();
  if (x->k != c) baz ();
  if (x->j != a.h) baz ();
}
