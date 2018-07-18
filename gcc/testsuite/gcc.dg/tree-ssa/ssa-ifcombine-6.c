/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ifcombine-details-blocks" } */

void bar (void);

void
foo1 (unsigned int a)
{
  if (a & 1)
    goto heaven;
  if (a & 4)
    goto heaven;
  return;

 heaven:
  bar ();
}

void
foo2 (unsigned int a)
{
  if (a & 1)
    if (a & 4)
      goto heaven;
  return;

 heaven:
  bar ();
}


/* The special treatment of a & 1 != 0 in fold caused the pattern not
   to be recognized due to extra conversions inserted.  */

/* { dg-final { scan-tree-dump "optimizing bits or bits test" "ifcombine" } } */
/* { dg-final { scan-tree-dump "optimizing double bit test" "ifcombine" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "ifcombine" } } */
