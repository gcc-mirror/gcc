/* { dg-options "-O2 -fdump-tree-dse-details -fno-tree-fre" } */


#ifndef SCOPE
#define SCOPE
#endif

extern void frob (char *);

void g (char *s)
{
  SCOPE char a[8];
  __builtin_strncpy (a, s, sizeof a);
  __builtin_memset (a, 0, sizeof a); 
  frob (a);
}

void h (char *s)
{
  SCOPE char a[8];
  __builtin_memset (a, 0, sizeof a); 
  __builtin_strncpy (a, s, sizeof a);
  frob (a);
}

void i (char *s)
{
  SCOPE char a[8];
  __builtin_strncpy (a, s, sizeof a);
  __builtin_memset (a, 0, sizeof a - 5); 
  frob (a);
}

void j (char *s)
{
  SCOPE char a[8];
  __builtin_memset (a, 0, sizeof a); 
  __builtin_strncpy (a, s, sizeof a - 5);
  frob (a);
}

void l (char *s)
{
  SCOPE char a[8];
  __builtin_strncpy (a, s, sizeof a);
  __builtin_memset (a + 2, 0, sizeof a - 2); 
  frob (a);
}

void m (char *s)
{
  SCOPE char a[8];
  __builtin_memset (a, 0, sizeof a); 
  __builtin_strncpy (a + 2, s, sizeof a - 2);
  frob (a);
}

/* { dg-final { scan-tree-dump-times "Deleted dead call" 2 "dse1" } } */
/* { dg-final { scan-tree-dump-times "Trimming statement " 4 "dse1" } } */

