/* PR tree-optimization/15826 - don't use "if" to extract a single bit
   bit-field */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct s {
  unsigned int bit : 1;
};

unsigned int
foo (struct s *p)
{
  if (p->bit)
    return 1;
  else
    return 0;
}

unsigned int
bar (struct s *p)
{
  return (unsigned int) (p->bit);
}

unsigned int
andrew (struct s *p)
{
  int i;
  if (p->bit)
    i = 1;
  else
    i = 0;
  return i;
}

/* { dg-final { scan-tree-dump-not "goto " "optimized" } } */
