/* PR middle-end/69542 */
/* { dg-do compile } */
/* { dg-additional-options "-fcompare-debug" } */

typedef struct A *B;
extern int *a[];
struct C { B b; struct D *d; };
struct A { struct { struct C e[1]; long long f[1]; } u; };
struct D { int g; B h[100]; };
int b, c, e, g;
B d, f;
void foo (void) __attribute__ ((__noreturn__));
int bar (void)
{
  int i = 0;
  do
    {
      if ('E' && a[e][0] != 'V')
        foo ();
      struct D *k = d->u.e[0].d;
      B x = k->h[i], o = f->u.e[0].b;
      if (b)
        return 0;
      if (a[g][0] != 'E' && a[g][0] != 'V')
        foo ();
      struct D *n = o->u.e[0].d;
      int r = x->u.f[0];
      (void) r;
      if (c)
        foo ();
      B y = n->h[x->u.f[0]];
      if (i != y->u.f[0])
        return 0;
      i++;
    }
  while (1);
}
