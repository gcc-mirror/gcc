/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre1" } */

struct s1
{
  int c;
};
struct s2
{
  struct s1 l;
};

void f(int fill)
{
  struct s1 D144916, last;
  int a = fill;
  D144916.c = a;
  struct s2 D135323;
  D135323.l = D144916;
  struct s1 *s = &D135323.l;
  const int *sc = &s->c;
  if (*sc != a)
    __builtin_abort();
}

/* We should be able to optimize out the condition guarding the abort.  */
/* { dg-final { scan-tree-dump-not "abort" "fre1" } } */
