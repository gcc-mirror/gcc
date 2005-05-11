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

/* Access to arr_base[iter].y should be strength reduced.  Depending on
   whether we have an addressing mode of type [base + offset], one of the
   following forms might get chosen:

   -- induction variable with base &arr_base[0].y, the memory access of
      form *iv = ...
   -- induction variable with base 0, the memory access of form
      *(iv + &arr_base[0].y) = ...

   In any case, we should not have 'arr_base[.*] ='  */

/* { dg-final { scan-tree-dump-times "arr_base\[.*\]\.y =" 0 "vars" } } */

/* And the original induction variable should be eliminated.  */

/* { dg-final { scan-tree-dump-times "iter" 0 "vars" } } */

/* { dg-final { cleanup-tree-dump "vars" } } */
