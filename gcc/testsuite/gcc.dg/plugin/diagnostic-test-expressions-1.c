/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret" } */

/* This is a collection of unittests to verify that we're correctly
   capturing the source code ranges of various kinds of expression.

   It uses the various "diagnostic_test_*_expression_range_plugin"
   plugins which handles "__emit_expression_range" by generating a warning
   at the given source range of the input argument.  Each of the
   different plugins do this at a different phase of the internal
   representation (tree, gimple, etc), so we can verify that the
   source code range information is valid at each phase.

   We want to accept an expression of any type.  To do this in C, we
   use variadic arguments, but C requires at least one argument before
   the ellipsis, so we have a dummy one.  */

extern void __emit_expression_range (int dummy, ...);

int global;

void test_parentheses (int a, int b)
{
  __emit_expression_range (0, (a + b) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, (a + b) );
                               ~~~^~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, (a + b) * (a - b) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, (a + b) * (a - b) );
                               ~~~~~~~~^~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, !(a && b) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, !(a && b) );
                               ^~~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Postfix expressions.  ************************************************/

void test_array_reference (int *arr)
{
  __emit_expression_range (0, arr[100] ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, arr[100] );
                               ~~~^~~~~
   { dg-end-multiline-output "" } */
}

int test_function_call (int p, int q, int r)
{
  __emit_expression_range (0, test_function_call (p, q, r) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, test_function_call (p, q, r) );
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  return 0;
}

struct test_struct
{
  int field;
};

int test_structure_references (struct test_struct *ptr)
{
  struct test_struct local;
  local.field = 42;

  __emit_expression_range (0, local.field ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, local.field );
                               ~~~~~^~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, ptr->field ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ptr->field );
                               ~~~^~~~~~~
   { dg-end-multiline-output "" } */
}

int test_postfix_incdec (int i)
{
  __emit_expression_range (0, i++ ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, i++ );
                               ~^~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, i-- ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, i-- );
                               ~^~
   { dg-end-multiline-output "" } */
}

/* Unary operators.  ****************************************************/

int test_prefix_incdec (int i)
{
  __emit_expression_range (0, ++i ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ++i );
                               ^~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, --i ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, --i );
                               ^~~
   { dg-end-multiline-output "" } */
}

void test_address_operator (void)
{
  __emit_expression_range (0, &global ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, &global );
                               ^~~~~~~
   { dg-end-multiline-output "" } */
}

void test_indirection (int *ptr)
{
  __emit_expression_range (0, *ptr ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, *ptr );
                               ^~~~
   { dg-end-multiline-output "" } */
}

void test_unary_minus (int i)
{
  __emit_expression_range (0, -i ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, -i );
                               ^~
   { dg-end-multiline-output "" } */
}

void test_ones_complement (int i)
{
  __emit_expression_range (0, ~i ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ~i );
                               ^~
   { dg-end-multiline-output "" } */
}

void test_logical_negation (int flag)
{
  __emit_expression_range (0, !flag ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, !flag );
                               ^~~~~
   { dg-end-multiline-output "" } */
}

/* Casts.  ****************************************************/

void test_cast (void *ptr)
{
  __emit_expression_range (0, (int *)ptr ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, (int *)ptr );
                               ^~~~~~~~~~
   { dg-end-multiline-output "" } */

}

/* Binary operators.  *******************************************/

void test_multiplicative_operators (int lhs, int rhs)
{
  __emit_expression_range (0, lhs * rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs * rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs / rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs / rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs % rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs % rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */
}

void test_additive_operators (int lhs, int rhs)
{
  __emit_expression_range (0, lhs + rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs + rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs - rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs - rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */
}

void test_shift_operators (int lhs, int rhs)
{
  __emit_expression_range (0, lhs << rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs << rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs >> rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs >> rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */
}

void test_relational_operators (int lhs, int rhs)
{
  __emit_expression_range (0, lhs < rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs < rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs > rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs > rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs <= rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs <= rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs >= rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs >= rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */
}

void test_equality_operators (int lhs, int rhs)
{
  __emit_expression_range (0, lhs == rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs == rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs != rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs != rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */
}

void test_bitwise_binary_operators (int lhs, int rhs)
{
  __emit_expression_range (0, lhs & rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs & rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs ^ rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs ^ rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs | rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs | rhs );
                               ~~~~^~~~~
   { dg-end-multiline-output "" } */
}

void test_logical_operators (int lhs, int rhs)
{
  __emit_expression_range (0, lhs && rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs && rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, lhs || rhs ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, lhs || rhs );
                               ~~~~^~~~~~
   { dg-end-multiline-output "" } */
}

/* Conditional operator.  *******************************************/

void test_conditional_operators (int flag, int on_true, int on_false)
{
  __emit_expression_range (0, flag ? on_true : on_false ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, flag ? on_true : on_false );
                               ~~~~~~~~~~~~~~~^~~~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Assignment expressions.  *******************************************/

void test_assignment_expressions (int dest, int other)
{
  __emit_expression_range (0, dest = other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest = other );
                               ~~~~~^~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest *= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest *= other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest /= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest /= other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest %= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest %= other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest += other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest += other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest -= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest -= other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest <<= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest <<= other );
                               ~~~~~^~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest >>= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest >>= other );
                               ~~~~~^~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest &= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest &= other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest ^= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest ^= other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, dest |= other ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dest |= other );
                               ~~~~~^~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Comma operator.  *******************************************/

void test_comma_operator (int a, int b)
{
  __emit_expression_range (0, (a++, a + b) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, (a++, a + b) );
                               ~~~~^~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Braced initializers.  ***************************************/

/* We can't test the ranges of these directly, since the underlying
   tree nodes don't retain a location.  However, we can test that they
   have ranges during parsing by building compound expressions using
   them, and verifying the ranges of the compound expressions.  */

#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

void test_braced_init (void)
{
  /* Verify start of range.  */
  __emit_expression_range (0, (vector(4, float)){2., 2., 2., 2.} + 1); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, (vector(4, float)){2., 2., 2., 2.} + 1);
                                                 ~~~~~~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */

  /* Verify end of range.  */
  __emit_expression_range (0, &(vector(4, float)){2., 2., 2., 2.}); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, &(vector(4, float)){2., 2., 2., 2.});
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Statement expressions.  ***************************************/

void test_statement_expression (void)
{
  __emit_expression_range (0, ({ static int a; a; }) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ({ static int a; a; }) );
                               ~^~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Other expressions.  */

void test_address_of_label (void)
{
 label:
  __emit_expression_range (0, &&label );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, &&label );
                               ^~~~~~~
   { dg-end-multiline-output "" } */
}

void test_transaction_expressions (void)
{
  int i;
  i = __transaction_atomic (42); /* { dg-error "without transactional memory support enabled" } */
/* { dg-begin-multiline-output "" }
   i = __transaction_atomic (42);
       ^~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  i = __transaction_relaxed (42); /* { dg-error "without transactional memory support enabled" } */
/* { dg-begin-multiline-output "" }
   i = __transaction_relaxed (42);
       ^~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_keywords (int i)
{
  __emit_expression_range (0, __FUNCTION__[i] );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __FUNCTION__[i] );
                               ~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, __PRETTY_FUNCTION__ );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __PRETTY_FUNCTION__ );
                               ^~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, __func__ );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __func__ );
                               ^~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_builtin_va_arg (__builtin_va_list v)
{
  __emit_expression_range (0,  __builtin_va_arg (v, int) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_va_arg (v, int) );
                                ~~~~~~~~~~~~~~~~~~~~~^~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,  __builtin_va_arg (v, int) + 1 );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_va_arg (v, int) + 1 );
                                ~~~~~~~~~~~~~~~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */
}

struct s
{
  int f;
};

void test_builtin_offsetof (int i)
{
  __emit_expression_range (0,  i + __builtin_offsetof (struct s, f) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  i + __builtin_offsetof (struct s, f) );
                                ~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,  __builtin_offsetof (struct s, f) + i );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_offsetof (struct s, f) + i );
                                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */
}

void test_builtin_choose_expr (int i)
{
  __emit_expression_range (0,  __builtin_choose_expr (1, i, i) + i);  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_choose_expr (1, i, i) + i);
                                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,  i + __builtin_choose_expr (1, i, i));  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  i + __builtin_choose_expr (1, i, i));
                                ~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

extern int f (int);
void test_builtin_call_with_static_chain (int i, void *ptr)
{
  __emit_expression_range (0, __builtin_call_with_static_chain (f (i), ptr));  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __builtin_call_with_static_chain (f (i), ptr));
                               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_builtin_complex (float i, float j)
{
  __emit_expression_range (0,  __builtin_complex (i, j) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_complex (i, j) );
                                ^~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

typedef int v4si __attribute__ ((vector_size (16)));
void test_builtin_shuffle (v4si a, v4si b, v4si mask)
{
  __emit_expression_range (0,  __builtin_shuffle (a, mask) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_shuffle (a, mask) );
                                ^~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,  __builtin_shuffle (a, b, mask) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_shuffle (a, b, mask) );
                                ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_alignof (int param)
{
  __emit_expression_range (0, __alignof__ (int) + param );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __alignof__ (int) + param );
                               ~~~~~~~~~~~~~~~~~~^~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,  param + __alignof__ (int) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  param + __alignof__ (int) );
                                ~~~~~~^~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,  __alignof__ (param) + param );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __alignof__ (param) + param );
                                ~~~~~~~~~~~~~~~~~~~~^~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,  param + __alignof__ (param) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  param + __alignof__ (param) );
                                ~~~~~~^~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Examples of non-trivial expressions.  ****************************/

extern double sqrt (double x);

void test_quadratic (double a, double b, double c)
{
  __emit_expression_range (0, b * b - 4 * a * c ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, b * b - 4 * a * c );
                               ~~~~~~^~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0,
     (-b + sqrt (b * b - 4 * a * c))
     / (2 * a)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
      (-b + sqrt (b * b - 4 * a * c))
      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      / (2 * a));
      ^~~~~~~~~
   { dg-end-multiline-output "" } */

}

/* Reproducer for PR c/68473.  */

extern long double fminl (long double __x, long double __y);
#define TEST_EQ(FUNC) FUNC##l(xl,xl)
void test_macro (long double xl)
{
  __emit_expression_range (0, TEST_EQ (fmin) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, TEST_EQ (fmin) );
                                        ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
 #define TEST_EQ(FUNC) FUNC##l(xl,xl)
                       ^~~~
   { dg-end-multiline-output "" } */
}

/* Verify that we can underline expressions that span multiple
   ordinary maps.  */

extern int foo (int, ...);

void test_multiple_ordinary_maps (void)
{
  /* The expression
        foo (0, "very long string...")
     below contains a transition between ordinary maps due to a very long
     line (>127 "columns", treating tab characters as 1 column).  */
  __emit_expression_range (0, foo (0, /* { dg-warning "range" } */
				   "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"));

/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, foo (0,
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"));
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  /* Another expression that transitions between ordinary maps; this
     one due to an ordinary map for a very long line transitioning back to
     one for a very short line.  The policy in linemap_line_start
     means that we need a transition from >10 bits of column
     (i.e. 2048 columns) to a line with <= 80 columns.  */
  __emit_expression_range (0, foo (0, "01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789", /* { dg-warning "range" } */
				   0));
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, foo (0, "01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789",
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        0));
        ~~                      
   { dg-end-multiline-output "" } */
}
