/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-additional-options "-fdump-tree-fre1" } */

int x;
int main()
{
  x = 0;
  if (x)
    {
      for (int i = 0; i < 10; ++i)
	x = i;
    }
  return x;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
