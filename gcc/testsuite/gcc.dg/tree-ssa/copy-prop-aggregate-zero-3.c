/* { dg-do compile } */ 
/* { dg-options "-O1 -fno-tree-sra -fdump-tree-optimized -fdump-tree-forwprop1-details" } */

extern void link_error (void);

struct g
{
  _Complex unsigned int t;
};

struct g f(void)
{
  struct g temp_struct1 ;
  temp_struct1.t = 0;
  struct g temp_struct2 = temp_struct1;
  struct g temp_struct3 = temp_struct2;
  struct g temp_struct4 = temp_struct3;
  return temp_struct4;
}

/* There should be no references to any of "temp_struct*"
   temporaries.  */
/* { dg-final { scan-tree-dump-times "temp_struct" 0 "optimized" } } */
/* Also check that forwprop pass did the copy prop. */
/* { dg-final { scan-tree-dump-times "after previous" 4 "forwprop1" } } */
