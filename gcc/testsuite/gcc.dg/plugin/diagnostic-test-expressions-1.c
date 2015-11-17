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
