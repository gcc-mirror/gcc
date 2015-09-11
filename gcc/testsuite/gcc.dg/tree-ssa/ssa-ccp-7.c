/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

extern void link_error (void);

/* tests to check if cprop works when using non-return functions   */

extern int not_returning (int) __attribute__ ((noreturn));

int b;
int test7 (int a)
{
  b = 7;
  if (a)
    {
      not_returning (a);
    }
  if (b != 7)
    link_error ();
  return b;
}


/* There should be not link_error calls, if there is any the
   optimization has failed */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
