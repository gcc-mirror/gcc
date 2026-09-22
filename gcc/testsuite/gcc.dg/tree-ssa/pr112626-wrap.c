/* PR tree-optimization/112626 - wrapping (-fwrapv) variant.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized" } */

#define FUNC(NAME, OP) \
int abs_##NAME (int a) { return __builtin_abs (a) OP a; }
#define FUNC_SWAPPED(NAME, OP) \
int a_##NAME##_abs (int a) { return a OP __builtin_abs (a); }
#define FUNC_U(NAME, OP) \
int abs_##NAME##_u (int a) { return ((unsigned) __builtin_abs (a)) OP (unsigned) a; }

FUNC (le, <=)
FUNC (lt, <)
FUNC (ge, >=)
FUNC (gt, >)
FUNC (eq, ==)
FUNC (ne, !=)

FUNC_SWAPPED (le, <=)
FUNC_SWAPPED (lt, <)
FUNC_SWAPPED (ge, >=)
FUNC_SWAPPED (gt, >)
FUNC_SWAPPED (eq, ==)
FUNC_SWAPPED (ne, !=)

FUNC_U (le, <=)
FUNC_U (gt, >)

/* { dg-final { scan-tree-dump-times "return 1;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 2147483648" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= 2147483648" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized" } } */
