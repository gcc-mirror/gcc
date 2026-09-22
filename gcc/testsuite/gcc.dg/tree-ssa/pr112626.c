/* PR tree-optimization/112626 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

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
FUNC_U (lt, <)
FUNC_U (ge, >=)
FUNC_U (gt, >)
FUNC_U (eq, ==)
FUNC_U (ne, !=)

/* { dg-final { scan-tree-dump-times "return 1;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " < 0" 6 "optimized" } } */
/* { dg-final { scan-tree-dump-times " >= 0" 6 "optimized" } } */
/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized" } } */
