/* PR tree-optimization/87096 - "Optimised" snprintf is not POSIX conformant
   Verify that calls to snprintf with size in excess of INT_MAX are not
   treated as successful.
   It would be valid for GCC to fold some of these calls to a negative
   value provided it also arranged to set errno to EOVERFLOW.  If that
   is ever implemented this test will need to be adjusted.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized -ftrack-macro-expansion=0" } */

#include "../range.h"

typedef __builtin_va_list va_list;

extern int snprintf (char*, size_t, const char*, ...);
extern int vsnprintf (char*, size_t, const char*, va_list);

#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to function named
     call_in_true_branch_not_eliminated_on_line_NNN()
   for each expression that's expected to fold to false but that
   GCC does not fold.  The dg-final scan-tree-dump-times directive
   at the bottom of the test verifies that no such call appears
   in output.  */
#define ELIM(expr)							\
  if ((expr)) FAIL (in_true_branch_not_eliminated); else (void)0

/* Macro to emit a call to a function named
     call_made_in_{true,false}_branch_on_line_NNN()
   for each call that's expected to be retained.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that the expected number of both kinds of calls appears in output
   (a pair for each line with the invocation of the KEEP() macro.  */
#define KEEP(expr)				\
  if (expr)					\
    FAIL (made_in_true_branch);			\
  else						\
    FAIL (made_in_false_branch)

extern void sink (int, ...);
#define sink(...) sink (0, __VA_ARGS__)

#define WARN(N, expr)				\
  do {						\
    char a[N];					\
    expr;					\
    sink (a);					\
  } while (0)


static const size_t imax = __INT_MAX__;
static const size_t imaxp1 = imax + 1;

#if __PTRDIFF_MAX__ == __INT_MAX__
/* Make the test pass on ILP32 the same way it does on LP64.  */
static const size_t dmax = __PTRDIFF_MAX__ + (size_t)1;
#else
static const size_t dmax = __PTRDIFF_MAX__;
#endif
static const size_t dmaxp1 = dmax + 1;

static const size_t szmax = __SIZE_MAX__;
static const size_t szmaxm1 = __SIZE_MAX__ - 1;


void test_size_cst (char **d)
{
  ELIM (0 > snprintf (*d++, imax, "%s", ""));

  KEEP (0 > snprintf (*d++, imaxp1, "%s", ""));   /* { dg-warning "\\\[-Wformat-truncation=]" } */

  KEEP (0 > snprintf (*d++, dmax, "%s", ""));     /* { dg-warning "\\\[-Wformat-truncation=]" } */
  KEEP (0 > snprintf (*d++, dmaxp1, "%s", ""));   /* { dg-warning "\\\[-Wformat-truncation=]" } */
  KEEP (0 > snprintf (*d++, szmaxm1, "%s", ""));  /* { dg-warning "\\\[-Wformat-truncation=]" } */
  KEEP (0 > snprintf (*d++, szmax, "%s", ""));    /* { dg-warning "\\\[-Wformat-truncation=]" } */
}


void test_size_cst_va (char **d, va_list va)
{
  ELIM (0 > vsnprintf (*d++, imax, " ", va));

  KEEP (0 > vsnprintf (*d++, imaxp1, " ", va));   /* { dg-warning "\\\[-Wformat-truncation=]" } */

  KEEP (0 > vsnprintf (*d++, dmax, " ", va));     /* { dg-warning "\\\[-Wformat-truncation=]" } */
  KEEP (0 > vsnprintf (*d++, dmaxp1, " ", va));   /* { dg-warning "\\\[-Wformat-truncation=]" } */
  KEEP (0 > vsnprintf (*d++, szmaxm1, " ", va));  /* { dg-warning "\\\[-Wformat-truncation=]" } */
  KEEP (0 > vsnprintf (*d++, szmax, " ", va));    /* { dg-warning "\\\[-Wformat-truncation=]" } */
}


void test_size_range (char **d)
{
  size_t r = UR (imax - 1, imax);
  ELIM (0 > snprintf (*d++, r, "%s", ""));

  r = UR (imax, imax + 1);
  KEEP (0 > snprintf (*d++, r, "%s", ""));

  r = UR (imaxp1, imaxp1 + 1);
  KEEP (0 > snprintf (*d++, r, "%s", ""));        /* { dg-warning "specified bound range \\\[\[0-9\]+, \[0-9\]+] exceeds .INT_MAX." } */

  r = UR (dmax, dmaxp1);
  KEEP (0 > snprintf (*d++, r, "%s", ""));        /* { dg-warning "\\\[-Wformat-truncation=]" } */

  r = UR (dmaxp1, dmaxp1 + 1);
  KEEP (0 > snprintf (*d++, r, "%s", ""));        /* { dg-warning "\\\[-Wformat-truncation=]" } */

  r = UR (szmaxm1, szmax);
  KEEP (0 > snprintf (*d++, r, "%s", ""));        /* { dg-warning "\\\[-Wformat-truncation=]" } */
}


void test_size_range_va (char **d, va_list va)
{
  size_t r = UR (imax - 1, imax);
  ELIM (0 > vsnprintf (*d++, r, " ", va));

  r = UR (imax, imax + 1);
  KEEP (0 > vsnprintf (*d++, r, " ", va));

  r = UR (imaxp1, imaxp1 + 1);
  KEEP (0 > vsnprintf (*d++, r, " ", va));        /* { dg-warning "specified bound range \\\[\[0-9\]+, \[0-9\]+] exceeds .INT_MAX." } */

  r = UR (dmax, dmaxp1);
  KEEP (0 > vsnprintf (*d++, r, " ", va));        /* { dg-warning "\\\[-Wformat-truncation=]" } */

  r = UR (dmaxp1, dmaxp1 + 1);
  KEEP (0 > vsnprintf (*d++, r, " ", va));        /* { dg-warning "\\\[-Wformat-truncation=]" } */

  r = UR (szmaxm1, szmax);
  KEEP (0 > vsnprintf (*d++, r, " ", va));        /* { dg-warning "\\\[-Wformat-truncation=]" } */
}


void test_size_varying (char **d, size_t n)
{
  KEEP (0 > snprintf (*d++, n, "%s", ""));

  n += 1;
  KEEP (0 > snprintf (*d++, n, "%s", ""));
}


void test_size_varying_va (char **d, size_t n, va_list va)
{
  KEEP (0 > vsnprintf (*d++, n, " ", va));

  n += 1;
  KEEP (0 > vsnprintf (*d++, n, " ", va));
}

/* { dg-final { scan-tree-dump-times " = snprintf" 12 "optimized"} }
   { dg-final { scan-tree-dump-times " = vsnprintf" 12 "optimized"} } */
