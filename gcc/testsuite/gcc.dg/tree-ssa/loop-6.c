/* { dg-do compile } */
/* { dg-options "-O1 -funswitch-loops -fdump-tree-unswitch-details -fdump-tree-vars" } */

int ch;
int a[100];

void xxx(void)
{
  int i;

  for (i = 0; i < 100; i++)
    {
      if (ch)
	a[i] = ch;
      else
	a[i] = i;
    }
}

/* Loop should be unswitched.  */

/* { dg-final { scan-tree-dump-times "Unswitching loop" 1 "unswitch" } } */

/* In effect there should be exactly three conditional jumps in the final program.  */

/* { dg-final { scan-tree-dump-times "else" 3 "vars" } } */
