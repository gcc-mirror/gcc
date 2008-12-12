/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-empty -fdump-tree-final_cleanup" } */

int foo (int n)
{
  while (n >= 45)
    n -= 45;

  return n;
}

int bar (int n)
{
  while (n >= 64)
    n -= 64;

  return n;
}

int bla (int n)
{
  int i = 0;

  while (n >= 45)
    {
      i++;
      n -= 45;
    }

  return i;
}

int baz (int n)
{
  int i = 0;

  while (n >= 64)
    {
      i++;
      n -= 64;
    }

  return i;
}

/* The loops computing division/modulo by 64 should be eliminated.  */
/* { dg-final { scan-tree-dump-times "Removing empty loop" 2 "empty" } } */

/* There should be no division/modulo in the final dump (division and modulo
   by 64 are done using bit operations).  */
/* { dg-final { scan-tree-dump-times "/" 0 "final_cleanup" } } */
/* { dg-final { scan-tree-dump-times "%" 0 "final_cleanup" } } */

/* { dg-final { cleanup-tree-dump "empty" } } */
/* { dg-final { cleanup-tree-dump "final_cleanup" } } */
