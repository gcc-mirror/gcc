/* { dg-do compile } */
/* { dg-options "-O -fstrict-aliasing -fdump-tree-dse1-details" } */

int a;
short *p;
void
test (int b)
{
  a=1;
  if (b)
    {
      (*p)++;
      a=2;
      __builtin_printf ("1\n");
    }
  else
    {
      (*p)++;
      a=3;
      __builtin_printf ("2\n");
    }
}

/* { dg-final { scan-tree-dump "Deleted dead store: a = 1;" "dse1" } } */
