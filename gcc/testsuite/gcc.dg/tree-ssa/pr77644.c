/* { dg-do compile } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -fdump-tree-optimized -funsafe-math-optimizations -fno-math-errno -ffinite-math-only" } */

#define FOO(type, cmp, suffix, no)  \
int f_##no(type x, type y) \
{ \
  type gen_##no(); \
  type xs = __builtin_sqrt##suffix((gen_##no())); \
  type xy = __builtin_sqrt##suffix((gen_##no())); \
  return (xs cmp xy); \
}

#define GEN_FOO(type, suffix) \
FOO(type, <, suffix, suffix##1) \
FOO(type, <=, suffix, suffix##2) \
FOO(type, >, suffix, suffix##3) \
FOO(type, >=, suffix, suffix##4) \
FOO(type, ==, suffix, suffix##5) \
FOO(type, !=, suffix, suffix##6)

GEN_FOO(float, f)
GEN_FOO(double, )
GEN_FOO(long double, l)

/* { dg-final { scan-tree-dump-not "__builtin_sqrtf" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_sqrt" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_sqrtl" "optimized" } } */
