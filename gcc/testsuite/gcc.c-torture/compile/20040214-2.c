/* http://gcc.gnu.org/ml/gcc-patches/2004-02/msg01307.html */

typedef struct xdef xdef;
struct xdef
{
  char xtyp;
  xdef *next;
  int y;
};

extern void b ();
extern void *foo (void *bar);
extern void *foo2 (void *bar1, void *bar2);
extern void *qwe;

static void
c (xdef * xp)
{
  b (xp);
}
static void
a (xdef ** xpp)
{
  xdef *xp;
  xp = *xpp;

  foo (xp);
  xp = foo2 (xp, qwe);
  b (xp->next);
  foo (xp);
  if (xp->y)
  {
    foo (xp);
    if (xp)
    {
      xdef *p = foo2 (xp, qwe);
      foo2 (xp, p);
      xp = foo (p);
    }
    else
    {
      foo2 (foo(*xpp), *xpp);
    }
  }
  *xpp = foo2 (xpp, qwe);
}

void
b (xdef ** xpp)
{
  xdef *xp = *xpp;
  if (!xp)
    return;
  if (xp->xtyp == 0)
    a (xpp);
  c (xp);
}
