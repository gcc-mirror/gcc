/* PR tree-optimization/86274 - SEGFAULT when logging std::to_string(NAN)
   { dg-do compile }
   { dg-skip-if "not IEEE float layout" { "pdp11-*-*" } }
   { dg-options "-O2 -Wall -fdump-tree-optimized" }  */

typedef __SIZE_TYPE__ size_t;
extern int sprintf (char*, const char*, ...);
extern int snprintf (char*, size_t, const char*, ...);

#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to funcation named
     call_in_true_branch_not_eliminated_on_line_NNN()
   for each expression that's expected to fold to false but that
   GCC does not fold.  The dg-final scan-tree-dump-time directive
   at the bottom of the test verifies that no such call appears
   in output.  */
#define ELIM(expr)							\
  if ((expr)) FAIL (in_true_branch_not_eliminated); else (void)0

/* Macro to emit a call to a function named
     call_made_in_{true,false}_branch_on_line_NNN()
   for each call that's expected to be retained.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
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


void test_elim (double x)
{
  ELIM (snprintf (0, 0, "%a", x) < 3);
  ELIM (snprintf (0, 0, "%e", x) < 3);
  ELIM (snprintf (0, 0, "%f", x) < 3);
  ELIM (snprintf (0, 0, "%g", x) < 1);

  /* Verify that snprintf knows that NaN cannot result in fewer
     than three characters on output.  */
  const double nan  = __builtin_nan ("0");
  ELIM (snprintf (0, 0, "%a", nan) < 3);
  ELIM (snprintf (0, 0, "%e", nan) < 3);
  ELIM (snprintf (0, 0, "%f", nan) < 3);
  ELIM (snprintf (0, 0, "%g", nan) < 3);
}

void test_keep (int p, double x)
{
  KEEP (snprintf (0, 0, "%a", x) == 3);
  KEEP (snprintf (0, 0, "%e", x) == 3);

  KEEP (snprintf (0, 0, "%f", x) == 3);
  KEEP (snprintf (0, 0, "%.*f", p, x) < 3);

  KEEP (snprintf (0, 0, "%g", x) == 1);
  KEEP (snprintf (0, 0, "%g", x) == 3);
}

void test_warn_sprintf_f (double x)
{
  WARN (4, sprintf (a, "%a", x));   /* { dg-warning "between 3 and 24 bytes" } */
  WARN (4, sprintf (a, "%e", x));   /* { dg-warning "between 3 and 14 bytes" } */
  WARN (4, sprintf (a, "%f", x));   /* { dg-warning "between 3 and 317 bytes" } */
  WARN (4, sprintf (a, "%g", x));   /* { dg-warning "between 1 and 13 bytes" } */
}


/* { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated_" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_in_true_branch_" 6 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_in_false_branch_" 6 "optimized" } } */
