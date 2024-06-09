// { dg-do compile }
// { dg-options "-g" }
struct A { struct A *a; } foo ();
struct B { long b; };
struct C { struct B c; struct A d; } *e;

void
bar (void)
{
  int f;
  struct C *g;
  struct A *h;
  for (g = 0, g = e ? (void *) e - (char) (__SIZE_TYPE__) &g->d : 0, h = g ? (&g->d)->a : 0; g;
       g = 0, g = h ? (void *) h - (char) (__SIZE_TYPE__) &g->d : 0, h = h ? h->a : 0)
    {
      f = (int) (__SIZE_TYPE__) g;
      foo (((struct B *) g)->b);
    }
}
