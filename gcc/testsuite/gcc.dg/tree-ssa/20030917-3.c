/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

extern int printf (const char *, ...); 

main ()
{
  int variable = 0;
  int p = 1;
  while (1)
    {
      if (p)
        break;
      variable = variable + 1;
      if (variable == 10)
        break;
    }
  printf("%d\n", variable);
}


/* The argument to "printf" should be a constant, not a variable.  */
/* { dg-final { scan-tree-dump-times "printf.*, 0" 1 "ccp1"} } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
