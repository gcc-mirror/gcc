/* Verify that we handle functions with __attribute__ ((const)) correctly.  */

#include "analyzer-decls.h"

extern int nonconst_fn (int);

extern int const_fn_0 () __attribute__ ((const));
extern int const_fn_1 (int) __attribute__ ((const));
extern int const_fn_2 (int, int) __attribute__ ((const));
extern int const_fn_3 (int, int, int) __attribute__ ((const));
extern int const_fn_variadic (int, ...) __attribute__ ((const));

/* Verify that functions without __attribute__ ((const)) have a different
   result each time.  */

void test_nonconst_fn (int x, int y)
{
  int x_1 = nonconst_fn (x);
  int x_2 = nonconst_fn (x);
  int y_1 = nonconst_fn (y);
  int y_2 = nonconst_fn (y);
  __analyzer_eval (x_1 == x_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (y_1 == y_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (x_1 == y_1); /* { dg-warning "UNKNOWN" } */
}

/* Verify functions with __attribute__ ((const)) have the same result
   for the same arguments.  */

 /* 0 args.  */

extern int other_const_fn_0 () __attribute__ ((const));

void test_const_fn_0 (void)
{
  int a = const_fn_0 ();
  int b = const_fn_0 ();
  int c = other_const_fn_0 ();
  int d = other_const_fn_0 ();
  __analyzer_eval (a == b); /* { dg-warning "TRUE" } */
  __analyzer_eval (c == d); /* { dg-warning "TRUE" } */
  __analyzer_eval (a == c); /* { dg-warning "UNKNOWN" } */
}

/* 1 arg.  */

void test_const_fn_1 (int x, int y)
{
  int x_1 = const_fn_1 (x);
  int x_2 = const_fn_1 (x);
  int y_1 = const_fn_1 (y);
  int y_2 = const_fn_1 (y);
  __analyzer_eval (x_1 == x_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (y_1 == y_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (x_1 == y_1); /* { dg-warning "UNKNOWN" } */
}

/* 2 args.  */

void test_const_fn_2 (int x, int y, int p, int q)
{
  int xy_1 = const_fn_2 (x, y);
  int xy_2 = const_fn_2 (x, y);
  int pq_1 = const_fn_2 (p, q);
  int pq_2 = const_fn_2 (p, q);
  __analyzer_eval (xy_1 == xy_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (pq_1 == pq_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (xy_1 == pq_1); /* { dg-warning "UNKNOWN" } */
}

/* We don't handle above 2 args.  */

void test_const_fn_3 (int x, int y, int z, int p, int q, int r)
{
  int xyz_1 = const_fn_3 (x, y, z);
  int xyz_2 = const_fn_3 (x, y, z);
  int pqr_1 = const_fn_3 (p, q, r);
  int pqr_2 = const_fn_3 (p, q, r);
  __analyzer_eval (xyz_1 == xyz_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (pqr_1 == pqr_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (xyz_1 == pqr_1); /* { dg-warning "UNKNOWN" } */
}

/* Variadic fn, with various numbers of extra args.  */

void test_const_fn_variadic (int x, int y, int z, int p, int q, int r)
{
  /* 0 extra args, for 1 arg in total.  */
  int x_1 = const_fn_variadic (x);
  int x_2 = const_fn_variadic (x);
  int p_1 = const_fn_variadic (p);
  int p_2 = const_fn_variadic (p);
  __analyzer_eval (x_1 == x_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (p_1 == p_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (x_1 == p_1); /* { dg-warning "UNKNOWN" } */

  /* 1 extra arg, for 2 args in total.  */
  int xy_1 = const_fn_variadic (x, y);
  int xy_2 = const_fn_variadic (x, y);
  int pq_1 = const_fn_variadic (p, q);
  int pq_2 = const_fn_variadic (p, q);
  __analyzer_eval (xy_1 == xy_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (pq_1 == pq_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (xy_1 == pq_1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (x_1 == xy_1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p_1 == pq_1); /* { dg-warning "UNKNOWN" } */

  /* Above that, we don't track results.  */
  int xyz_1 = const_fn_variadic (x, y, z);
  int xyz_2 = const_fn_variadic (x, y, z);
  int pqr_1 = const_fn_variadic (p, q, r);
  int pqr_2 = const_fn_variadic (p, q, r);
  __analyzer_eval (xyz_1 == xyz_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (pqr_1 == pqr_2); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (xyz_1 == x_1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (xyz_1 == xy_1); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (xyz_1 == pqr_1); /* { dg-warning "UNKNOWN" } */
}

/* Builtins with __attribute__ ((const)).  */

void test_builtin_isascii (int x, int y)
{
  int x_1 = __builtin_isascii (x);
  int x_2 = __builtin_isascii (x);
  int y_1 = __builtin_isascii (y);
  int y_2 = __builtin_isascii (y);
  __analyzer_eval (x_1 == x_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (y_1 == y_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (x_1 == y_1); /* { dg-warning "UNKNOWN" } */
}

void test_builtin_popcount (unsigned x, unsigned y)
{
  unsigned x_1 = __builtin_popcount (x);
  unsigned x_2 = __builtin_popcount (x);
  unsigned y_1 = __builtin_popcount (y);
  unsigned y_2 = __builtin_popcount (y);
  __analyzer_eval (x_1 == x_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (y_1 == y_2); /* { dg-warning "TRUE" } */
  __analyzer_eval (x_1 == y_1); /* { dg-warning "UNKNOWN" } */
}

void test_loop (void)
{
  for (int i = 0; i < 100; i++)
    {
      int iter_val_a = const_fn_1 (i);
      int iter_val_b = const_fn_1 (i);
      __analyzer_eval (iter_val_a == iter_val_b); /* { dg-warning "TRUE" } */
    }
}
