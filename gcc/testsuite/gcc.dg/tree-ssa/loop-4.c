/* A test for strength reduction and induction variable elimination.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-vars" } */

/* Size of this structure should be sufficiently weird so that no memory
   addressing mode applies.  */

struct bla
{
  char x[187];
  int y;
  char z[253];
} arr_base[100];

int foo(void);

void xxx(void)
{
  int iter;

  for (iter = 0; iter < 100; iter++)
    arr_base[iter].y = foo ();
}

/* Access to arr_base[iter].y should be strength reduced.  */

/* { dg-final { scan-tree-dump-times "arr_base\[^\\n\\r\]*=" 0 "vars" } } */

/* And the original induction variable should be eliminated.  */

/* { dg-final { scan-tree-dump-times "iter" 0 "vars" } } */
