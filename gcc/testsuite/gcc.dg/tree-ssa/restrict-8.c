/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

struct s
{
  int *__restrict__ *__restrict__ pp;
};

int
f (struct s s, int *b)
{
  *b = 1;
  **s.pp = 2;
  return *b;
}

/* { dg-final { scan-tree-dump-times "return 1" 1 "fre1" } } */
