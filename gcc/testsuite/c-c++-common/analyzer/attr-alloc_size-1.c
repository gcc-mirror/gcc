#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef __SIZE_TYPE__ size_t;

extern void* (*my_alloc)(size_t) __attribute__ ((alloc_size (1)));
extern void* (*my_alloc_2)(size_t, size_t) __attribute__ ((alloc_size (1, 2)));

int test_one_arg_concrete_int_ptr (void)
{
  int *x = (int *) my_alloc (1); /* { dg-warning "allocated buffer size is not a multiple of the pointee's size" } */
  __analyzer_dump_capacity (x); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)1'" } */
  x[0] = 0; /* { dg-warning "buffer overflow" } */
  return 0;
}

void test_one_arg_concrete (void)
{
  char *p = (char *) my_alloc (10);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)10'" } */
  p[0] = 'a';
  p[9] = 'b';
  p[10] = 'c'; /* { dg-warning "buffer overflow" } */
  p[11] = 'c'; /* { dg-warning "buffer overflow" } */
}

void test_one_arg_symbolic (size_t sz)
{
  char *p = (char *) my_alloc (sz);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(sz" } */
}

void test_two_args_concrete (void)
{
  char *p = (char *) my_alloc_2 (2, 5);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)10'" } */
  p[0] = 'a';
  p[9] = 'b';
  p[10] = 'c'; /* { dg-warning "buffer overflow" } */
  p[11] = 'c'; /* { dg-warning "buffer overflow" } */
}

void test_two_args_symbolic_first (size_t sz)
{
  char *p = (char *) my_alloc_2 (sz, 5);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(INIT_VAL\\(sz\[^\n\r\]*\\*\\(size_t\\)5\\)'" } */
}

void test_two_args_symbolic_second (size_t sz)
{
  char *p = (char *) my_alloc_2 (5, sz);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(INIT_VAL\\(sz\[^\n\r\]*\\*\\(size_t\\)5\\)'" } */
}

void test_two_args_symbolic_both (size_t a, size_t b)
{
  char *p = (char *) my_alloc_2 (a, b);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(INIT_VAL\\(a\[^\n\r\]*\\*INIT_VAL\\(b" } */
}

typedef void* (*my_alloc_t)(size_t) __attribute__ ((alloc_size (1)));
typedef void* (*my_alloc_2_t)(size_t, size_t) __attribute__ ((alloc_size (1, 2)));

void test_one_arg_concrete_fnptr (my_alloc_t fnptr)
{
  char *p = (char *) fnptr (10);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)10'" } */
  p[0] = 'a';
  p[9] = 'b';
  p[10] = 'c'; /* { dg-warning "buffer overflow" } */
  p[11] = 'c'; /* { dg-warning "buffer overflow" } */
}

void test_two_args_concrete_fnptr (my_alloc_2_t fnptr)
{
  char *p = (char *) fnptr (2, 5);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)10'" } */
  p[0] = 'a';
  p[9] = 'b';
  p[10] = 'c'; /* { dg-warning "buffer overflow" } */
  p[11] = 'c'; /* { dg-warning "buffer overflow" } */
}
