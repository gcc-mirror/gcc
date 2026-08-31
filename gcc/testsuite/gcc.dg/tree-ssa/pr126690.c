/* { dg-do compile } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

__attribute__((noinline))
_Complex float
cproj_idempotentf (_Complex float a)
{
  return __builtin_cprojf (__builtin_cprojf (a));
}

__attribute__((noinline))
_Complex double
cproj_idempotent (_Complex double a)
{
  return __builtin_cproj (__builtin_cproj (a));
}

__attribute__((noinline))
_Complex long double
cproj_idempotentl (_Complex long double a)
{
  return __builtin_cprojl (__builtin_cprojl (a));
}

__attribute__((noinline))
float
cabs_absorbs_cprojf (_Complex float a)
{
  return __builtin_cabsf (__builtin_cprojf (a));
}

__attribute__((noinline))
double
cabs_absorbs_cproj (_Complex double a)
{
  return __builtin_cabs (__builtin_cproj (a));
}

__attribute__((noinline))
long double
cabs_absorbs_cprojl (_Complex long double a)
{
  return __builtin_cabsl (__builtin_cprojl (a));
}

/* { dg-final { scan-tree-dump-times "__builtin_cproj" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_cabs" 3 "optimized" } } */
