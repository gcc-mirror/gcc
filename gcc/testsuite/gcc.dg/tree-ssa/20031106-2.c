/* { dg-do compile } */ 
/* { dg-options "-O1 -fdump-tree-optimized" } */

extern void link_error (void);

/* Check for dead stores to a struct.  */

struct s
{
  char d;
  int a, b;
  double m;
};

void foo (struct s* teststruct)
{
  teststruct->a = 0;
  teststruct->a++;
  if (teststruct->a != 1)
    link_error ();
}

/* There should be only one reference to "teststruct" and one in the function header.  */
/* { dg-final { scan-tree-dump-times "teststruct" 2 "optimized" } } */

/* There should be no link_error calls.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */

