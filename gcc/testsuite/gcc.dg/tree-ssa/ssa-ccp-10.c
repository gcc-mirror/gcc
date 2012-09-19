/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-fab1" } */

/* Check that we fold strlen of equally long strings, and that we do not
   fail to terminate when there is a nontrivial cycle in the corresponding
   ssa graph.  */

extern __SIZE_TYPE__ strlen (const char *);

void foo(int i)
{
  char *s = "abcde";

  if (i)
    {
      s = "defgh";
      goto middle;
    }

start:

  bla ();

middle:

  if (bla ())
    goto start;

  bar (strlen (s));
}

/* There should be no calls to strlen.  */
/* { dg-final { scan-tree-dump-times "strlen" 0 "fab1"} } */
/* { dg-final { cleanup-tree-dump "fab1" } } */
