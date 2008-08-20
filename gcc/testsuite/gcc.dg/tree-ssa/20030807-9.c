/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2" } */

static void
bar ()
{
  const char *label2 = (*"*.L_sfnames_b" == '*') + "*.L_sfnames_b";
  oof (label2);
}

void
ooof ()
{
  if (""[0] == 0)
    foo();
}

/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dom2"} } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
