/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void bar(char *);

/* Ensure that PTR1 = PTR2 + OFF properly picks up the zero and non-zero
   properties if PTR1 and PTR2 are known equal or non-equal.  */

void foo1 (char *p, char *pp, int off)
{
  char *q = p + off;
  if (q != p)
    {
      if (off == 0)
	  bar (q);
    }
  else
    {
      if (off != 0)
        bar (p);
    }
}

void foo2 (char *p, char *pp, int off)
{
  char *q = p + off;
  if (q == p)
    {
      if (off != 0)
        bar (p);
    }
  else
    {
      if (off == 0)
	  bar (q);
    }
}

/* { dg-final { scan-tree-dump-not "bar" "evrp" } } */
