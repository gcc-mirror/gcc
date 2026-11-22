/* PR target/64897 */
/* { dg-do compile } */
/* { dg-require-effective-target double64 } */
/* { dg-skip-if "not IEEE" { vax-*-* pdp11-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define F(name, T, I, OP, C)			\
  T name (T x)					\
  {						\
    I i;					\
    __builtin_memcpy (&i, &x, sizeof i);	\
    i OP C;					\
    __builtin_memcpy (&x, &i, sizeof x);	\
    return x;					\
  }

F (abs_f, float, __UINT32_TYPE__, &=, 0x7fffffffu)
F (abs_d, double, __UINT64_TYPE__, &=, 0x7fffffffffffffffull)
F (neg_f, float, __UINT32_TYPE__, ^=, 0x80000000u)
F (neg_d, double, __UINT64_TYPE__, ^=, 0x8000000000000000ull)

/* { dg-final { scan-tree-dump-times "ABS_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = -x_" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "optimized" } } */
