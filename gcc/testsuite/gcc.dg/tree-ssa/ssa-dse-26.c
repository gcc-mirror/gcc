/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details -fno-short-enums -fno-tree-fre" } */
/* { dg-skip-if "we want a BIT_FIELD_REF from fold_truth_andor" { ! lp64 } } */
/* { dg-skip-if "temporary variable names are not x and y" { mmix-knuth-mmixware } } */

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

/* { dg-final { scan-tree-dump-times "Deleted dead store: x = " 1 "dse1" } } */
/* { dg-final { scan-tree-dump-times "Deleted dead store: y = " 1 "dse1" } } */
