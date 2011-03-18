/* PR bootstrap/48161 */

struct T { int u; };
struct G { int l; int t; int r; };
struct V { struct G v[10]; };
struct { struct V b; } *h;
void bar (void);

struct G *
baz (struct V *x, unsigned y)
{
  return &x->v[y];
}

int
foo (struct T *x, struct T *y)
{
  if ((baz (&h->b, y->u)->t ? baz (&h->b, y->u)->t : 0)
      - baz (h ? &h->b : 0, x->u)->r
      - (baz (h ? &h->b : 0, x->u)->t > 0 ? 5 : 0))
    return 1;
  bar ();
  return 0;
}
