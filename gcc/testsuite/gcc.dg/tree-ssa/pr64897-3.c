/* PR target/64897 */
/* { dg-do compile } */
/* { dg-require-effective-target double64 } */
/* { dg-options "-O2 -fsignaling-nans -fdump-tree-optimized" } */

/* With signaling NaNs honored the integer operations stay: abs and
   negation can be done in an FP unit that quiets them.  */

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
F (nabs_f, float, __UINT32_TYPE__, |=, 0x80000000u)
F (nabs_d, double, __UINT64_TYPE__, |=, 0x8000000000000000ull)

/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-not " = -x_" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\.COPYSIGN" "optimized" } } */
