/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-forwprop1-details" } */

struct s1
{
  int t[1024];
};

void f(struct s1);

void g(struct s1 a)
{
  struct s1 temp_struct0 = a;
  f(temp_struct0);
}

/* There should be no references to any of "temp_struct*"
   temporaries.  */
/* { dg-final { scan-tree-dump-times "temp_struct" 0 "optimized" } } */
/* Also check that forwprop pass did the copy prop. */
/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */
