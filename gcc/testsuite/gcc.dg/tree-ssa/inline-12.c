/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-einline" } */

void *foo (void *, int);
static inline void *mcp (void *src, int i)
{
  return foo (src, i);
}
void bar()
{
  int i;
  mcp (&i, 0);
}

/* There should be exactly two assignments, one for both
   the original foo call and the inlined copy (plus a clobber
   that doesn't match here).  In particular bar should look like
     <bb 2> :
     _4 = foo (&i, 0);
     i ={v} {CLOBBER};
     return;  */
/* { dg-final { scan-tree-dump-times " = " 2 "einline" } } */
