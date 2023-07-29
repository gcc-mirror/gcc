/* PR tree-optimization/100864 */

/* { dg-do run } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */

#define op_ne !=
#define op_eq ==
#define op_lt <
#define op_le <=
#define op_gt >
#define op_ge >=

#define operators(t) \
t(ne) \
t(eq) \
t(lt) \
t(le) \
t(gt) \
t(ge)

#define cmpfunc(v, op) \
__attribute__((noipa)) \
_Bool func_##op##_##v(v int a, v int b, v _Bool e) \
{ \
  v _Bool c = (a op_##op b); \
  v _Bool d = !c; \
  return (e & d) | c; \
}

#define cmp_funcs(op) \
cmpfunc(, op) \
cmpfunc(volatile , op)

operators(cmp_funcs)

#define test(op) \
if (func_##op##_ (a, b, e) != func_##op##_volatile (a, b, e)) \
 __builtin_abort();

int main()
{
  for(int a = -3; a <= 3; a++)
    for(int b = -3; b <= 3; b++)
      {
       _Bool e = 0;
       operators(test)
       e = 1;
       operators(test)
      }
  return 0;
}

/* Check to make sure we optimize `(a&!b) | b` -> `a | b`. */
/* There are 6 different comparison operators testing here. */
/* bit_not_expr and bit_and_expr should show up for each one (volatile). */
/* Each operator should show up twice
   (except for `!=` which shows up 2*6 (each tester) + 2 (the 2 loops) extra = 16). */
/* bit_ior_expr will show up for each operator twice (non-volatile and volatile). */
/* { dg-final { scan-tree-dump-times "ne_expr,"      16 "optimized"} } */
/* { dg-final { scan-tree-dump-times "eq_expr,"       2 "optimized"} } */
/* { dg-final { scan-tree-dump-times "lt_expr,"       2 "optimized"} } */
/* { dg-final { scan-tree-dump-times "le_expr,"       2 "optimized"} } */
/* { dg-final { scan-tree-dump-times "gt_expr,"       2 "optimized"} } */
/* { dg-final { scan-tree-dump-times "ge_expr,"       2 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_not_expr,"  6 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_and_expr,"  6 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr," 12 "optimized"} } */
