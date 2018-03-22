/* PR bootstrap/82916 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-dse" } */

struct A { struct A *next; };
struct C
{
  int *of;
  struct C *parent, *prev, *next;
  int depth;
  int min;
  struct C *min_occ;
};

__attribute__((noipa)) struct C *
foo (int *node)
{
  struct A *p = __builtin_malloc (sizeof (struct C));
  if (!p)
    return 0;
  p->next = 0;
  /* Originally placement new.  */
  struct C *nw = (struct C *)(void *)p;
  nw->of = node;
  nw->parent = 0;
  nw->prev = 0;
  nw->next = 0;
  nw->depth = 0;
  nw->min_occ = nw;
  nw->min = 0;
  return nw;
}

int
main ()
{
  int o;
  struct C *p = foo (&o);
  if (p)
    {
      if (p->of != &o || p->parent || p->prev || p->next || p->depth
	  || p->min || p->min_occ != p)
	__builtin_abort ();
    }
  __builtin_free (p);
  return 0;
}
