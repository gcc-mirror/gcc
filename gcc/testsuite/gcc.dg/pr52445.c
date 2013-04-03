/* PR tree-optimization/52445 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-cselim -fdump-tree-cselim" } */

void
foo (char *buf, unsigned long len)
{
  buf[0] = '\n';
  if (len > 1)
    buf[1] = '\0';	/* We can't cselim "optimize" this, while
			   buf[0] doesn't trap, buf[1] could.  */
}

/* { dg-final { scan-tree-dump-not "cstore\." "cselim" } } */
/* { dg-final { cleanup-tree-dump "cselim" } } */
