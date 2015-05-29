/* PR middle-end/23848 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void bar (char *, char *, char *, char *, int);
void foo (int size)
{
  char temp[size];
  temp[size-1] = '\0';
  {
    char temp2[size];
    {
      char temp3[size];
      {
	char temp4[size];
	bar (temp, temp2, temp3, temp4, size);
      }
    }
    bar (temp, temp2, (char *) 0, (char *) 0, size);
  }
}

/* { dg-final { scan-tree-dump-times "__builtin_stack_save" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "__builtin_stack_restore" 1 "optimized"} } */
