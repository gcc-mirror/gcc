/* { dg-do compile } */
/* { dg-options "-O -Wmaybe-uninitialized -fdump-tree-uninit1" } */

extern unsigned bar (void);
extern void quux (void);
int z;
unsigned foo (unsigned v, int y, int w)
{
  unsigned u;
  if (v != 1)
    u = bar ();

  // Prevent the "dom" pass from changing the CFG layout based on the inference
  // 'if (v != 1) is false then (v != 2) is true'.  (Now it would have to
  // duplicate the loop in order to do so, which is deemed expensive.)
  for (int i = 0; i < 10; i++)
    quux ();

  // This variantion from uninit-pred-11.c caused compute_control_dep_chain
  // to run into a defect, producing z != 0 && v != 1, omitting !(i<10)
  // from the path predicate
  if (w)
    {
      if (y)
	z = 1;
      if (v != 1)
	return u;       /* { dg-bogus "may be used uninitialized" } */
    }

  return 0;
}

/* Make sure predicate analysis picked up the loop exit condition.  */
/* { dg-final { scan-tree-dump "AND \\(NOT \\((ivtmp|doloop)" "uninit1" } } */
