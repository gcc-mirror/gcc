/* { dg-require-effective-target alloca } */

#include <stdlib.h>

/* Tests warn on use of floating-point operands inside the calculation
   of an allocation size.

   The test cases here only test for warnings.  The test cases inside
   allocation-size-X.c should be plently enough to test for false positives.  */

void test_1 (float f)
{
  int *ptr = malloc (sizeof (int) * f); /* { dg-line test_1 } */
  free (ptr);

  /* { dg-warning "use of floating-point arithmetic here might yield unexpected results" "warning" { target *-*-* } test_1 } */
  /* { dg-message "operand 'f' is of type 'float'" "note" { target *-*-* } test_1 } */
  /* { dg-message "only use operands of an integer type inside the size argument" "note" { target *-*-* } test_1 } */
}

void test_2 (int n)
{
  int *ptr = malloc (n * 3.1); /* { dg-line test_2 } */
  free (ptr);

  /* { dg-warning "use of floating-point arithmetic here might yield unexpected results" "warning" { target *-*-* } test_2 } */
  /* { dg-message "operand '\(\\d|e|f|\\.|\\+|\)+' is of type 'double'" "note" { target *-*-* } test_2 } */
  /* { dg-message "only use operands of an integer type inside the size argument" "note" { target *-*-* } test_2 } */
}

void *alloc_me (size_t size)
{
  return malloc (size); /* { dg-line test_3 } */

  /* { dg-warning "use of floating-point arithmetic here might yield unexpected results" "warning" { target *-*-* } test_3 } */
  /* { dg-message "operand 'f' is of type 'float'" "note" { target *-*-* } test_3 } */
  /* { dg-message "only use operands of an integer type inside the size argument" "note" { target *-*-* } test_3 } */
}

void test_3 (float f)
{
  void *ptr = alloc_me (f); /* { dg-message "calling 'alloc_me' from 'test_3'" } */
  free (ptr);
}

void test_4 (int n)
{
  int *ptr = calloc(1.7 * n, sizeof (int)); /* { dg-line test_4 } */
  free (ptr);

  /* { dg-warning "use of floating-point arithmetic here might yield unexpected results" "warning" { target *-*-* } test_4 } */
  /* { dg-message "operand '\(\\d|e|f|\\.|\\+|\)+' is of type 'double'" "note" { target *-*-* } test_4 } */
  /* { dg-message "only use operands of an integer type inside the size argument" "note" { target *-*-* } test_4 } */
}

int test_5 (float f)
{
  int *ptr = __builtin_alloca (sizeof (int) * f); /* { dg-line test_5 } */
  *ptr = 4;
  return *ptr;

  /* { dg-warning "use of floating-point arithmetic here might yield unexpected results" "warning" { target *-*-* } test_5 } */
  /* { dg-message "operand 'f' is of type 'float'" "note" { target *-*-* } test_5 } */
  /* { dg-message "only use operands of an integer type inside the size argument" "note" { target *-*-* } test_5 } */
}

int test_6 (float f)
{
  int *ptr = __builtin_alloca (1.7f * f * 2.3f); /* { dg-line test_6 } */
  *ptr = 4;
  return *ptr;

  /* { dg-warning "use of floating-point arithmetic here might yield unexpected results" "warning" { target *-*-* } test_6 } */
  /* { dg-message "operand 'f' is of type 'float'" "note" { target *-*-* } test_6 } */
  /* { dg-message "only use operands of an integer type inside the size argument" "note" { target *-*-* } test_6 } */
}
