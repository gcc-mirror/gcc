/* This testcase ICEd when 2 different successors of a basic block
   were successfully threaded and try_forward_edges was not expecting
   that.  */

typedef struct A
{
  struct A *s, *t;
  unsigned int u;
} A;

void bar (A *);

void
foo (A *x, A *y, A *z)
{
  while (y
	 && (((y && y->t && y->t->u) ? y : z)->t
	     == ((x && x->t && x->t->u) ? x : z)->t))
    y = y->s;

  if (y)
    bar (y);
}
