/* { dg-options "-O2 -fdump-tree-dse-details -fno-tree-fre" } */


struct S { int i; char n[128]; int j; };

void f (char*);

void g (struct S *p)
{
  char a[sizeof p->n + 1];

  __builtin_memset (a, 0, sizeof a);   // dead store, can be eliminated

  __builtin_strncpy (a, p->n, sizeof a - 1);
  a[sizeof a - 1] = '\0';

  f (a);
}

/* { dg-final { scan-tree-dump-times "Deleted dead call" 1 "dse1" } } */
