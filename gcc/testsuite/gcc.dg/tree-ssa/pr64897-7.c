/* PR target/64897 */
/* { dg-do compile } */
/* { dg-require-effective-target double64 } */
/* { dg-skip-if "not IEEE" { vax-*-* pdp11-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Setting the sign bit is -abs (x), which is copysign (x, -1) on targets
   that have copysign.  */

#define F(name, T, I, OP, C)			\
  T name (T x)					\
  {						\
    I i;					\
    __builtin_memcpy (&i, &x, sizeof i);	\
    i OP C;					\
    __builtin_memcpy (&x, &i, sizeof x);	\
    return x;					\
  }

F (nabs_f, float, __UINT32_TYPE__, |=, 0x80000000u)
F (nabs_d, double, __UINT64_TYPE__, |=, 0x8000000000000000ull)

/* { dg-final { scan-tree-dump-times "ABS_EXPR|\\.COPYSIGN" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR" "optimized" } } */
