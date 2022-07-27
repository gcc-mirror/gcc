#include "analyzer-decls.h"
typedef __SIZE_TYPE__ size_t;

int test_1 (void)
{
  int i; /* { dg-message "region created on stack here" } */
  return i; /* { dg-warning "use of uninitialized value 'i'" } */
}

int test_2 (void)
{
  int i; /* { dg-message "region created on stack here" } */
  return i * 2; /* { dg-warning "use of uninitialized value 'i'" } */
}

int test_3 (void)
{
  static int i;
  return i;
}

int test_4 (void)
{
  int *p; /* { dg-message "region created on stack here" } */
  return *p; /* { dg-warning "use of uninitialized value 'p'" } */
}

int test_5 (int flag, int *q)
{
  int *p; /* { dg-message "region created on stack here" } */
  if (flag) /* { dg-message "following 'false' branch" } */
    p = q;

  /* There should be two enodes here,
     i.e. not merging the init vs non-init states.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
  
  return *p; /* { dg-warning "use of uninitialized value 'p'" } */
}

int test_6 (int i)
{
  int arr[10]; /* { dg-message "region created on stack here" } */
  return arr[i]; /* { dg-warning "use of uninitialized value 'arr\\\[i\\\]'" } */
}

int test_rshift_rhs (int i)
{
  int j; /* { dg-message "region created on stack here" } */
  return i >> j; /* { dg-warning "use of uninitialized value 'j'" } */
}

int test_lshift_rhs (int i)
{
  int j; /* { dg-message "region created on stack here" } */
  return i << j; /* { dg-warning "use of uninitialized value 'j'" } */
}

int test_rshift_lhs (int i)
{
  int j; /* { dg-message "region created on stack here" } */
  return j >> i; /* { dg-warning "use of uninitialized value 'j'" } */
}

int test_lshift_lhs (int i)
{
  int j; /* { dg-message "region created on stack here" } */
  return j << i; /* { dg-warning "use of uninitialized value 'j'" } */
}

int test_cmp (int i)
{
  int j; /* { dg-message "region created on stack here" } */
  return i < j; /* { dg-warning "use of uninitialized value 'j'" } */
}

float test_plus_rhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return x + y; /* { dg-warning "use of uninitialized value 'y'" } */
}

float test_plus_lhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return y + x; /* { dg-warning "use of uninitialized value 'y'" } */
}

float test_minus_rhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return x - y; /* { dg-warning "use of uninitialized value 'y'" } */
}

float test_minus_lhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return y - x; /* { dg-warning "use of uninitialized value 'y'" } */
}

float test_times_rhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return x * y; /* { dg-warning "use of uninitialized value 'y'" } */
}

float test_times_lhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return y * x; /* { dg-warning "use of uninitialized value 'y'" } */
}

float test_divide_rhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return x / y; /* { dg-warning "use of uninitialized value 'y'" } */
}

float test_divide_lhs (float x)
{
  float y; /* { dg-message "region created on stack here" } */
  return y / x; /* { dg-warning "use of uninitialized value 'y'" } */
}

size_t test_builtin_strlen (void)
{
  const char *ptr; /* { dg-message "region created on stack here" } */
  return __builtin_strlen (ptr); /* { dg-warning "use of uninitialized value 'ptr'" } */
}

void test_calling_uninit_fn_ptr_1 (void)
{
  void (*fn_ptr) (void); /* { dg-message "region created on stack here" } */
  fn_ptr (); /* { dg-warning "use of uninitialized value 'fn_ptr'" } */
}

int test_calling_uninit_fn_ptr_2 (void)
{
  int (*fn_ptr) (void); /* { dg-message "region created on stack here" } */
  return fn_ptr (); /* { dg-warning "use of uninitialized value 'fn_ptr'" } */
}

extern void called_by_uninit_arg (int);
void test_passing_uninit_arg (void)
{
  int i; /* { dg-message "region created on stack here" } */
  called_by_uninit_arg (i); /* { dg-warning "use of uninitialized value 'i'" } */
}
