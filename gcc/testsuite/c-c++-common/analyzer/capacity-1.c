/* { dg-require-effective-target alloca } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <stdlib.h>
#include "../../gcc.dg/analyzer/analyzer-decls.h"

typedef unsigned __INT32_TYPE__ u32;

void
test_1 (void)
{
  char buf[16];
  __analyzer_dump_capacity (buf); /* { dg-warning "capacity: '\\(sizetype\\)16'" } */
}

void
test_2 (void)
{
  char ch;
  __analyzer_dump_capacity (&ch); /* { dg-warning "capacity: '\\(sizetype\\)1'" } */
}

struct s3 { char buf[100]; };

void
test_3 (void)
{
  struct s3 s;
  __analyzer_dump_capacity (&s); /* { dg-warning "capacity: '\\(sizetype\\)100'" } */
}

/* Capacity refers to the base region, not any offset within it.  */

void
test_4 (void)
{
  char buf[1024];
  __analyzer_dump_capacity (buf + 100); /* { dg-warning "capacity: '\\(sizetype\\)1024'" } */
}

void
test_5 (void *p)
{
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" } */
}

void
test_malloc (void)
{
  void *p = malloc (1024);

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(size_t\\)1024'" } */
  free (p);
}

void
test_alloca (size_t sz)
{
  void *p = __builtin_alloca (sz);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(sz_\[^\n\r\]*\\)'" } */
}

void
test_vla (size_t sz)
{
  char buf[sz];
  __analyzer_dump_capacity (buf);  /* { dg-warning "capacity: 'INIT_VAL\\(sz_\[^\n\r\]*\\)'" } */
}

static void * __attribute__((noinline))
called_by_test_interproc_malloc (size_t a)
{
  return malloc (a);
}

void *
test_interproc_malloc (size_t sz)
{
  void *p = called_by_test_interproc_malloc (sz);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(sz_\[^\n\r\]*\\)'" } */
  return p;
}

struct s
{
  u32 f1;
  char arr[];
};

static struct s * __attribute__((noinline))
alloc_s (size_t num)
{
  struct s *p = (struct s *) malloc (sizeof(struct s) + num);
  return p;
}

struct s *
test_trailing_array (void)
{
  struct s *p = alloc_s (5);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)9'" } */
  return p;
}

void
test_unknown_arr (int p[])
{
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" } */
}
