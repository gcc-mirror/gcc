/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

/* Transitive relations which the bidirectional relation search has to find.  */

void keep (void);
void kill (void);

/* The plain chain:  A < B < C.  */
void
f1 (int a, int b, int c)
{
  if (a < b)
    if (b < c)
      {
	if (a < c)
	  keep ();
	else
	  kill ();
      }
}

/* As f1, but with an unrelated relation on A recorded in a block between the
   two which matter.  Walking up the dominator tree has to continue past the
   first block holding a relation for A rather than stopping there.  */
void
f2 (int a, int b, int c, int d)
{
  if (a < b)
    if (b < c)
      if (a < d)
	{
	  if (a < c)
	    keep ();
	  else
	    kill ();
	}
}

/* The same pair queried several times in one block.  The first query memoizes
   its answer in the block, so the later ones have to merge with that record
   rather than create a second one.  */
void
f3 (int a, int b, int c, int d)
{
  if (a < b)
    if (b < c)
      if (a < d)
	{
	  if (a < c)
	    keep ();
	  else
	    kill ();
	  if (c > a)
	    keep ();
	  else
	    kill ();
	  if (a != c)
	    keep ();
	  else
	    kill ();
	  if (a <= c)
	    keep ();
	  else
	    kill ();
	}
}

/* A <= C <= B gives A <= B, so the true edge of A != B is A < B.  */
void
f4 (int a, int b, int c)
{
  if (a <= c)
    if (c <= b)
      if (a != b)
	{
	  if (a < b)
	    keep ();
	  else
	    kill ();
	}
}

/* A chain long enough that the two ends have to meet in the middle.  */
void
f5 (int a, int b, int c, int d, int e)
{
  if (a < b)
    if (b < c)
      if (c < d)
	if (d < e)
	  {
	    if (a < e)
	      keep ();
	    else
	      kill ();
	  }
}

/* { dg-final { scan-tree-dump-not "kill" "evrp" } } */
/* { dg-final { scan-tree-dump-times "keep" 8 "evrp" } } */
