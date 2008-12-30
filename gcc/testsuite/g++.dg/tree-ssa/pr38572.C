// PR tree-optimization/38572
// { dg-do compile }
// { dg-options "-O2" }

// Crash caused by the out-of-bounds enum values (all the remaining cruft
// is needed only to trigger the appropriate code path in tree-vrp.c).
enum JSOp
{
  JSOP_GETELEM = 5,
  JSOP_LIMIT
};
extern void g ();
void f (char *pc, char *endpc, int format, char ***fp, enum JSOp op)
{
  while (pc <= endpc)
    {
      if ((fp && *fp && pc == **fp) || pc == endpc)
	{
	  if (format == 1)
	    op = (JSOp) 256;
	  else if (format == 2)
	    op = (JSOp) 257;
	  else
	    op = JSOP_GETELEM;
	}
      if (op >= JSOP_LIMIT)
	{
	  if (format)
	    g ();
	}
    }
}
