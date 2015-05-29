/* PR tree-optimization/52448 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-cselim -fdump-tree-cselim-details" } */

extern void perhaps_free_something (void);

void f1 (int *p, int a, int b, int cond, int cond2)
{
  *p = a;
  if (cond)
    perhaps_free_something ();
  if (cond2)
    *p = b;
}

void f2 (int *p, int a, int b, int *cond, int *cond2)
{
  int i;
  *p = a;
  for (i = 0; cond[i]; i++)
    {
      if (cond2[i])
        *p = b;
      perhaps_free_something ();
    }
}

/* None of the above conditional stores might be made unconditional.  */
/* { dg-final { scan-tree-dump-not "cstore" "cselim" } } */
