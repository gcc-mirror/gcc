/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread1-details -fdisable-tree-ethread" } */

extern void abort (void);

typedef union tree_node *tree;
union tree_node
{
  int code;
  tree chain;
  int omp_code;
}
bitmap_head;

extern int c_omp_predetermined_sharing (tree);

tree
c_finish_omp_clauses (tree clauses)
{
  tree c, t, *pc = &clauses;
  for (pc = &clauses, c = clauses; c; c = *pc)
    {
      unsigned char remove = 0;
      switch (((c->omp_code)))
	{
	case 1:
	  if (t->code != 42)
	    remove = 1;
	  switch (c_omp_predetermined_sharing (t))
	    {
	    case 2:
	      abort ();
	    }
	}
      if (remove)
	*pc = c->chain;
    }
}

/* There are 3 FSM jump threading opportunities, two of which will
  get filtered out.  */
/* { dg-final { scan-tree-dump-times "Registering FSM" 1 "thread1"} } */
/* { dg-final { scan-tree-dump-times "FSM Thread through multiway branch without threading a multiway branch" 2 "thread1"} } */
