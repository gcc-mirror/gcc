/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

struct A
{
  int a;
  int b;
};

foo (struct A *p, struct A *q)
{
  int *r = 0;

  if (p)
    {
      if (p == q)
	{
	  /* This should be folded to 'if (1)' because q is [p, p]
	     and p is ~[0, 0].  */
	  if (q)
	    r = &q->a;

	  /* This should be folded to 'if (1)' because q should be
	     ~[0, 0] and thus &q->a should be ~[0, 0].  */
	  if (r)
	    return 5;
	}
    }

  return q->a;
}

/* { dg-final { scan-tree-dump-times "Folding predicate q_.*to 1" 1 "vrp" } } */
/* { dg-final { scan-tree-dump-times "Folding predicate r_.*to 1" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
