/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret -fpermissive" } */

/* This is a collection of unittests to verify that we're correctly
   capturing the source code ranges of various kinds of expression.

   It uses the various "diagnostic_test_*_expression_range_plugin"
   plugins which handles "__emit_expression_range" by generating a warning
   at the given source range of the input argument.  Each of the
   different plugins do this at a different phase of the internal
   representation (tree, gimple, etc), so we can verify that the
   source code range information is valid at each phase.

   We want to accept an expression of any type.  We use variadic arguments.
   For compatibility with the C tests we have a dummy argument, since
   C requires at least one argument before the ellipsis.  */

extern void __emit_expression_range (int dummy, ...);

int global;

void test_global (void)
{
  __emit_expression_range (0, global); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, global);
                               ^~~~~~
   { dg-end-multiline-output "" } */
}

void test_param (int param)
{
  __emit_expression_range (0, param); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, param);
                               ^~~~~
   { dg-end-multiline-output "" } */
}

void test_local (void)
{
  int local = 5;

  __emit_expression_range (0, local); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, local);
                               ^~~~~
   { dg-end-multiline-output "" } */
}

void test_integer_constants (void)
{
  __emit_expression_range (0, 1234); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 1234);
                               ^~~~
   { dg-end-multiline-output "" } */

  /* Ensure that zero works.  */

  __emit_expression_range (0, 0); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 0);
                               ^
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, -273); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, -273);
                               ^~~~
   { dg-end-multiline-output "" } */

}

void test_character_constants (void)
{
  __emit_expression_range (0, 'a'); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 'a');
                               ^~~
   { dg-end-multiline-output "" } */
}

void test_floating_constants (void)
{
  __emit_expression_range (0, 98.6); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 98.6);
                               ^~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, .6); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, .6);
                               ^~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, 98.); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 98.);
                               ^~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, 6.022140857e23 ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 6.022140857e23 );
                               ^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, 98.6f ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 98.6f );
                               ^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, 6.022140857e23l ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 6.022140857e23l );
                               ^~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, -273.15f); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, -273.15f);
                               ^~~~~~~~
   { dg-end-multiline-output "" } */

}

enum test_enum {
  TEST_ENUM_VALUE
};

void test_enumeration_constant (void)
{
  __emit_expression_range (0, TEST_ENUM_VALUE ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, TEST_ENUM_VALUE );
                               ^~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

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
                               ~~~~~~~^
   { dg-end-multiline-output "" } */
}

int test_function_call (int p, int q, int r)
{
  __emit_expression_range (0, test_function_call (p, q, r) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, test_function_call (p, q, r) );
                               ~~~~~~~~~~~~~~~~~~~^~~~~~~~~
   { dg-end-multiline-output "" } */
  return 0;
}

struct test_struct
{
  int field;
};

void test_structure_references (struct test_struct *ptr)
{
  struct test_struct local;
  local.field = 42;

  __emit_expression_range (0, local.field ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, local.field );
                               ~~~~~~^~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, ptr->field ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ptr->field );
                               ~~~~~^~~~~
   { dg-end-multiline-output "" } */
}

void test_postfix_incdec (int i)
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

void test_sizeof (int i)
{
  __emit_expression_range (0, sizeof i ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, sizeof i );
                               ^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, sizeof (char) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, sizeof (char) );
                               ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_alignof (int i)
{
  __emit_expression_range (0, alignof(int)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, alignof(int));
                               ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, __alignof__(int)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __alignof__(int));
                               ^~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  __emit_expression_range (0, __alignof__ i); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __alignof__ i);
                               ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_prefix_incdec (int i)
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

void test_unary_plus (int i)
{
  __emit_expression_range (0, +i ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, +i );
                               ^~
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

  __emit_expression_range (0, *(int *)0xdeadbeef ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, *(int *)0xdeadbeef );
                               ^~~~~~~~~~~~~~~~~~
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
                               ~~~~~^~~~~~~~~~~~~~~~~~~~
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

/* Literals.  **************************************************/

void test_string_literals ()
{
  __emit_expression_range (0, "0123456789"); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, "0123456789");
                               ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, "foo" "bar" ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, "foo" "bar" );
                               ^~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_numeric_literals (int i)
{
  __emit_expression_range (0, 42 ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 42 );
                               ^~
   { dg-end-multiline-output "" } */

  /* Verify locations of negative literals (via folding of
     unary negation).  */

  __emit_expression_range (0, -42 ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, -42 );
                               ^~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, i ? 0 : -1 ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, i ? 0 : -1 );
                               ~~^~~~~~~~
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
  __emit_expression_range (0, 1 + (vector(4, float)){2., 2., 2., 2.}); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 1 + (vector(4, float)){2., 2., 2., 2.});
                               ~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Statement expressions.  ***************************************/

void test_statement_expression (void)
{
  __emit_expression_range (0, ({ static int a; a; }) + 1);  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ({ static int a; a; }) + 1);
                               ~~~~~~~~~~~~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, 1 + ({ static int a; a; }) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, 1 + ({ static int a; a; }) );
                               ~~^~~~~~~~~~~~~~~~~~~~~~~~
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
                               ~~~~~~~~~~~~~~^
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, __PRETTY_FUNCTION__[i] );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __PRETTY_FUNCTION__[i] );
                               ~~~~~~~~~~~~~~~~~~~~~^
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, __func__[i] );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, __func__[i] );
                               ~~~~~~~~~~^
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

struct s { int i; float f; };

void test_builtin_offsetof (int i)
{
  __emit_expression_range (0,  __builtin_offsetof (struct s, f) );  /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0,  __builtin_offsetof (struct s, f) );
                                ~~~~~~~~~~~~~~~~~~~~^~~~~~~~~~~~
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

int bar (int);
void test_combinations (int a)
{
  __emit_expression_range (0, bar (a) > a ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, bar (a) > a );
                               ~~~~~~~~^~~
   { dg-end-multiline-output "" } */
}

/* C++-specific expresssions. ****************************************/

void test_cp_literal_keywords (int a, int b)
{
  this; /* { dg-error "invalid use of 'this' in non-member function" } */
/* { dg-begin-multiline-output "" }
   this;
   ^~~~
   { dg-end-multiline-output "" } */

}

class base {
 public:
  base ();
  base (int i);
  virtual ~base ();
  int pub ();
private:
  int priv ();
};
class derived : public base { ~derived (); };

void test_cp_casts (base *ptr)
{
  __emit_expression_range (0, dynamic_cast <derived *> (ptr)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, dynamic_cast <derived *> (ptr));
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, static_cast <derived *> (ptr)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, static_cast <derived *> (ptr));
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, reinterpret_cast <int *> (ptr)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, reinterpret_cast <int *> (ptr));
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, const_cast <base *> (ptr)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, const_cast <base *> (ptr));
                               ^~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_functional_casts (int i, float f)
{
  __emit_expression_range (0, float(i)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, float(i));
                               ^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, int(f)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, int(f));
                               ^~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, s{i, f}); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, s{i, f});
                               ^~~~~~~
   { dg-end-multiline-output "" } */
}

template <typename TYPENAME>
class example_template
{
public:
  example_template (TYPENAME v);
};

void test_template_id (void)
{
  example_template <int>; /* { dg-warning "declaration does not declare anything" } */
/* { dg-begin-multiline-output "" }
   example_template <int>;
   ^~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_new (void)
{
  __emit_expression_range (0, ::new base); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ::new base);
                               ^~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, new base); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, new base);
                               ^~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, new (base)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, new (base));
                               ^~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, new base (42)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, new base (42));
                               ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, new (base) (42)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, new (base) (42));
                               ^~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */

  /* TODO: placement new.  */

  __emit_expression_range (0, new example_template<int> (42)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, new example_template<int> (42));
                               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

void test_methods ()
{
  __emit_expression_range (0, ((base *)1)->pub () ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, ((base *)1)->pub () );
                               ~~~~~~~~~~~~~~~~~^~
   { dg-end-multiline-output "" } */

  ((base *)1)->priv (); // { dg-error " is private " }
/* { dg-begin-multiline-output "" }
   ((base *)1)->priv ();
   ~~~~~~~~~~~~~~~~~~^~
   { dg-end-multiline-output "" }
   { dg-begin-multiline-output "" }
   int priv ();
       ^~~~
   { dg-end-multiline-output "" } */
}

class tests
{
public:
  void test_method_calls ();
  int some_method () const;
};

void
tests::test_method_calls ()
{
  __emit_expression_range (0, this->some_method () ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, this->some_method () );
                               ~~~~~~~~~~~~~~~~~~^~
   { dg-end-multiline-output "" } */
}

namespace std
{
  class type_info { public: int foo; };
}

void test_typeid (int i)
{
  __emit_expression_range (0, typeid(i)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, typeid(i));
                               ^~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, typeid(int)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, typeid(int));
                               ^~~~~~~~~~~
   { dg-end-multiline-output "" } */

  __emit_expression_range (0, typeid(i * 2)); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, typeid(i * 2));
                               ^~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

/* Various tests of locations involving macros.  */

void test_within_macro_1 (int lhs, int rhs)
{
#define MACRO_1(EXPR) EXPR

  __emit_expression_range (0, MACRO_1 (lhs == rhs));

/* { dg-warning "range" "" { target *-*-* } .-2 } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, MACRO_1 (lhs == rhs));
                                        ~~~~^~~~~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
 #define MACRO_1(EXPR) EXPR
                       ^~~~
   { dg-end-multiline-output "" } */

#undef MACRO_1
}

void test_within_macro_2 (int lhs, int rhs)
{
#define MACRO_2(EXPR) EXPR

  __emit_expression_range (0, MACRO_2 (MACRO_2 (lhs == rhs)));

/* { dg-warning "range" "" { target *-*-* } .-2 } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, MACRO_2 (MACRO_2 (lhs == rhs)));
                                                 ~~~~^~~~~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, MACRO_2 (MACRO_2 (lhs == rhs)));
                                        ^~~~~~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
 #define MACRO_2(EXPR) EXPR
                       ^~~~
   { dg-end-multiline-output "" } */

#undef MACRO_2
}

void test_within_macro_3 (int lhs, int rhs)
{
#define MACRO_3(EXPR) EXPR

  __emit_expression_range (0, MACRO_3 (lhs) == MACRO_3 (rhs));

/* { dg-warning "range" "" { target *-*-* } .-2 } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, MACRO_3 (lhs) == MACRO_3 (rhs));
                                             ^
   { dg-end-multiline-output "" } */

#undef MACRO_3
}

void test_within_macro_4 (int lhs, int rhs)
{
#define MACRO_4(EXPR) EXPR

  __emit_expression_range (0, MACRO_4 (MACRO_4 (lhs) == MACRO_4 (rhs)));

/* { dg-warning "range" "" { target *-*-* } .-2 } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range (0, MACRO_4 (MACRO_4 (lhs) == MACRO_4 (rhs)));
                                                      ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
 #define MACRO_4(EXPR) EXPR
                       ^~~~
   { dg-end-multiline-output "" } */

#undef MACRO_4
}
