/* { dg-do link }  */
/* { dg-options "-O2 -fdump-tree-vrp1" }  */

void link_error (void);

static inline void do_thing(char *s, int *p, char *q)
{
  	/* This should be folded away.  */
        if (s == 0 || q == 0)
                link_error ();

	/* This should not be folded as 'p' is not marked nonnull.  */
	if (p)
	  	*p = 3;
}

void __attribute__((nonnull (1, 3))) do_other_thing(char *s, int *p, char *q)
{
        do_thing(s, p, q);
}

int i;

int
main()
{
  do_other_thing ("xxx", &i, "yyy");
}

/* { dg-final { scan-tree-dump-times "Folding predicate p_.*" 0 "vrp1" } } */
