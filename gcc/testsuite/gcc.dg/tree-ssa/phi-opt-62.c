/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-phiopt2" } */

/* phiopt removes the conditional block, which binds W.  The bind is
   moved before the condition and reset, so that W does not keep the
   value of A on the path through the block.  */

unsigned
f (unsigned m, unsigned a, unsigned b, _Bool c)
{
  unsigned w = a;
  unsigned r = m;
  if (c)
    {
      w = b;
      r = m | w;
    }
  return r;
}

/* { dg-final { scan-tree-dump "DEBUG w => NULL" "phiopt2" } } */
