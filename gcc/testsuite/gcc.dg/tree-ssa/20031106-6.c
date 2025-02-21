/* { dg-do compile } */ 
/* { dg-options "-O1 -fno-tree-sra -fdump-tree-optimized -fdump-tree-forwprop1-details" } */

/* PR tree-optimization/14295 */

extern void link_error (void);

/* Check for copyprop on structs.  */

struct s
{
  char d;
  int a, b;
  double m;
};

struct s foo (struct s r)
{
  struct s temp_struct1;
  struct s temp_struct2;
  struct s temp_struct3;
  temp_struct1 = r;
  temp_struct2 = temp_struct1;
  temp_struct3 = temp_struct2;
  return temp_struct3;
}

/* There should be no references to any of "temp_struct*"
   temporaries.  */
/* { dg-final { scan-tree-dump-times "temp_struct" 0 "optimized" } } */
/* Also check that forwprop pass did the copy prop. */
/* { dg-final { scan-tree-dump-times "after previous" 3 "forwprop1" } } */
