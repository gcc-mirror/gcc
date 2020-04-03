/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details -fno-short-enums -fno-tree-fre" } */
/* { dg-skip-if "temporary variable for constraint_expr is never used" { msp430-*-* } } */

enum constraint_expr_type
{
  SCALAR, DEREF, ADDRESSOF
};
typedef struct constraint_expr
{
  enum constraint_expr_type type;
  unsigned int var;
  long offset;
} constraint_expr ;
typedef struct constraint
{
  struct constraint_expr lhs;
  struct constraint_expr rhs;
} constraint;
static _Bool
constraint_expr_equal (struct constraint_expr x, struct constraint_expr y)
{
  return x.type == y.type && x.var == y.var && x.offset == y.offset;
}

_Bool
constraint_equal (struct constraint a, struct constraint b)
{
  return constraint_expr_equal (a.lhs, b.lhs)
    && constraint_expr_equal (a.rhs, b.rhs);
}

/* Most targets should be using this test.  */
/* { dg-final { scan-tree-dump-times "Deleted dead store: x = " 1 "dse1" { target { ! tic6x-*-* } } } } */
/* { dg-final { scan-tree-dump-times "Deleted dead store: y = " 1 "dse1" { target { ! tic6x-*-* } } } } */

/* The c6x port generates significantly different gimple which
   changes the SRA and DSE decisions.   Verify we remove all
   dead stores.  */
/* { dg-final { scan-tree-dump-times "Deleted dead store: \[ax\].. = " 2 "dse1" { target tic6x-*-* } } } */
/* { dg-final { scan-tree-dump-times "Deleted dead store: \[by\].. = " 2 "dse1" { target tic6x-*-* } } } */

