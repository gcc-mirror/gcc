/* Copyright (C) 2010  Free Software Foundation.

   Verify that folding of built-in cproj is correctly performed by the
   compiler.  With -ffinite-math-only all cproj calls should be
   eliminated regardless of what the argument is, or what is known
   about it.

   Origin: Kaveh R. Ghazi,  April 9, 2010.  */

/* { dg-do link } */
/* { dg-options "-ffinite-math-only" } */

/* All references to link_error should go away at compile-time.  The
   argument is the __LINE__ number.  It appears in the tree dump file
   and aids in debugging should any of the tests fail.  */
extern void link_error(int);

#define CPROJ(X) __builtin_cproj(X)
#define CPROJF(X) __builtin_cprojf(X)
#define CPROJL(X) __builtin_cprojl(X)

/* Test that the supplied expressions eliminte the cproj call.  */
#define TEST_EXPRS(LD_EXPR, D_EXPR, F_EXPR) do { \
  if (CPROJF(F_EXPR) != (F_EXPR)) \
    link_error (__LINE__); \
  if (CPROJ(D_EXPR) != (D_EXPR)) \
    link_error (__LINE__); \
  if (CPROJL(LD_EXPR) != (LD_EXPR)) \
    link_error (__LINE__); \
} while (0)

void foo (_Complex long double cld, _Complex double cd, _Complex float cf)
{
#ifdef __OPTIMIZE__
  TEST_EXPRS (cld, cd, cf);
  TEST_EXPRS (cld*2, cd*2, cf*2);
  TEST_EXPRS (cld*cld, cd*cd, cf*cf);
#endif

  return;
}

int main (void)
{
  return 0;
}
