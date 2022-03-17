/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

typedef struct boxed_int { int value; } boxed_int;

extern boxed_int boxed_int_add (boxed_int a, boxed_int b);
extern boxed_int boxed_int_mul (boxed_int a, boxed_int b);

boxed_int  __attribute__((noinline))
noinline_boxed_int_add (boxed_int a, boxed_int b)
{
  boxed_int result;
  result.value = a.value + b.value;
  return result;
}

static inline boxed_int
inline_boxed_int_add (boxed_int a, boxed_int b)
{
  boxed_int result;
  result.value = a.value + b.value;
  return result;
}

boxed_int
test_1 (boxed_int a, boxed_int b)
{
  boxed_int result = boxed_int_add (boxed_int_mul (a, a),
				    boxed_int_mul (b, b));
  return result;
}

void
test_2a (void)
{
  boxed_int arr[4];
  arr[0].value = 1;
  arr[1].value = 2;
  arr[2].value = 3;
  arr[3].value = 4;
  boxed_int sum;
  sum.value = arr[0].value + arr[1].value + arr[2].value + arr[3].value;
  __analyzer_eval (sum.value == 10); /* { dg-warning "TRUE" } */
}

void
test_2b (void)
{
  boxed_int a, b, c, d;
  a.value = 1;
  b.value = 2;
  c.value = 3;
  d.value = 4;
  boxed_int sum;
  sum.value = a.value + b.value + c.value + d.value;
  __analyzer_eval (sum.value == 10); /* { dg-warning "TRUE" } */
}

void
test_2c (void)
{
  boxed_int a, b, c, d;
  a.value = 1;
  b.value = 2;
  c.value = 3;
  d.value = 4;
  boxed_int sum = inline_boxed_int_add (inline_boxed_int_add (a, b),
					inline_boxed_int_add (c, d));
  __analyzer_eval (sum.value == 10); /* { dg-warning "TRUE" } */
}

void
test_2d (void)
{
  boxed_int a, b, c, d;
  a.value = 1;
  b.value = 2;
  c.value = 3;
  d.value = 4;
  boxed_int sum = noinline_boxed_int_add (noinline_boxed_int_add (a, b),
					  noinline_boxed_int_add (c, d));
  __analyzer_eval (sum.value == 10); /* { dg-warning "TRUE" } */
}

/* Pointer to a local.  */

void test_4 (void)
{
  boxed_int i;
  int *p = &i.value;
  i.value = 1;
  *p = 2;
  __analyzer_eval (i.value == 2); /* { dg-warning "TRUE" } */
}

/* Local array.  */

void test_5 (void)
{
  boxed_int a[10];
  a[3].value = 5; /* ARRAY_REF.  */
  __analyzer_eval (a[3].value == 5); /* { dg-warning "TRUE" } */
}

/* Local array, but using an unknown index.  */

void test_5a (int idx)
{
  boxed_int a[10];
  a[idx].value = 5; /* ARRAY_REF.  */
  __analyzer_eval (a[idx].value == 5); /* { dg-warning "TRUE" } */
}

/* Array passed in as a param.  */

void test_6 (boxed_int a[10])
{
  /* POINTER_PLUS_EXPR then a MEM_REF.  */
  __analyzer_eval (a[3].value == 42); /* { dg-warning "UNKNOWN" } */
  a[3].value = 42;
  __analyzer_eval (a[3].value == 42); /* { dg-warning "TRUE" } */
}

/* Array passed in as a param ptr.  */

void test_7 (boxed_int *a)
{
  __analyzer_eval (a[3].value == 42); /* { dg-warning "UNKNOWN" } */
  a[3].value = 42;
  __analyzer_eval (a[3].value == 42); /* { dg-warning "TRUE" } */
}

/* Globals.  */

boxed_int glob_a;

void test_10 (void)
{
  __analyzer_eval (glob_a.value == 42); /* { dg-warning "UNKNOWN" } */
  glob_a.value = 42;
  __analyzer_eval (glob_a.value == 42); /* { dg-warning "TRUE" } */
}

/* Use of uninit value.  */
int test_12a (void)
{
  boxed_int i; /* { dg-message "region created on stack here" } */
  return i.value; /* { dg-warning "use of uninitialized value 'i.value'" } */
}

/* Use of uninit value.  */
boxed_int test_12b (void)
{
  boxed_int i; /* { dg-message "region created on stack here" } */
  return i; /* { dg-warning "use of uninitialized value '\[^\n\r\]*'" } */
}

void test_loop (void)
{
  boxed_int i;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  for (i.value=0; i.value<256; i.value++) {
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
