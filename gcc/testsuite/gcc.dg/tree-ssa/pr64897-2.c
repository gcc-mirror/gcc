/* PR target/64897 */
/* { dg-do compile } */
/* { dg-require-effective-target double64 } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Only clearing, flipping or setting the sign bit alone is abs, negation
   or -abs.  */

#define F(name, T, I, OP, C)			\
  T name (T x)					\
  {						\
    I i;					\
    __builtin_memcpy (&i, &x, sizeof i);	\
    i OP C;					\
    __builtin_memcpy (&x, &i, sizeof x);	\
    return x;					\
  }

F (and_f, float, __UINT32_TYPE__, &=, 0x7ffffffeu)
F (xor_f, float, __UINT32_TYPE__, ^=, 0x40000000u)
F (ior_f, float, __UINT32_TYPE__, |=, 0x40000000u)
F (and_d, double, __UINT64_TYPE__, &=, 0x7fffffff7fffffffull)
F (xor_d, double, __UINT64_TYPE__, ^=, 0x80000000ull)
F (ior_d, double, __UINT64_TYPE__, |=, 0x80000000ull)

/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-not " = -x_" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\.COPYSIGN" "optimized" } } */
