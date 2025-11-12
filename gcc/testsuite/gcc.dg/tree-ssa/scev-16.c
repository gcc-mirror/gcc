/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-sccp" } */

extern char a[];
int foo ()
{
  int cnt = 0;
  char *aend = a + 32;
  char *a0 = a;
  do
    {
      a0 = a0 + 16;
      cnt++;
    }
  while (aend - a0 > 12);
  return cnt;
}

/* { dg-final { scan-tree-dump "return 2" "sccp" } } */
