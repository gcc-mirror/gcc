#include "analyzer-decls.h"

struct st
{
  void *ptr[10];
  int arr[10];
};

/* Various combinations of a pair of writes, involving
   symbolic vs concrete clusters, with symbolic vs concrete keys
   within them.  */

struct st g;

/* "ptr" write: fully concrete.  */

struct st
test_conc_conc_ptr_conc_conc_arr (void)
{
  struct st s;
  s.ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  s.arr[5] = 42;
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  __analyzer_describe (0, s.arr[5]);  /* { dg-warning "42" } */
  return s;
}

struct st
test_conc_conc_ptr_conc_sym_arr (int j)
{
  struct st s;
  s.ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  s.arr[j] = 42;
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, s.arr[j]);  /* { dg-warning "42" } */
  return s;
}

struct st
test_conc_conc_ptr_sym_conc_arr (struct st *p)
{
  struct st s;
  s.ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  p->arr[5] = 42;
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  __analyzer_describe (0, p->arr[5]);  /* { dg-warning "42" } */
  return s;
}

struct st
test_conc_conc_ptr_sym_sym_arr (struct st *p, int j)
{
  struct st s;
  s.ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  p->arr[j] = 42;
  __analyzer_describe (0, s.ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  __analyzer_describe (0, p->arr[j]);  /* { dg-warning "42" } */
  return s;
}

/* "ptr" write: symbolic region, but at concrete offset.  */

void
test_sym_conc_ptr_conc_conc_arr (struct st *p)
{
  p->ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  g.arr[5] = 42;
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, g.arr[5]);  /* { dg-warning "42" } */
}

void
test_sym_conc_ptr_conc_sym_arr (struct st *p, int j)
{
  p->ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  g.arr[j] = 42;
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, g.arr[j]);  /* { dg-warning "42" } */
}

void
test_sym_conc_ptr_sym_conc_arr (struct st *p, struct st *q)
{
  p->ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  q->arr[5] = 42;
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, q->arr[5]);  /* { dg-warning "42" } */
}

void
test_sym_conc_ptr_sym_sym_arr (struct st *p, struct st *q, int j)
{
  p->ptr[1] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  q->arr[j] = 42;
  __analyzer_describe (0, p->ptr[1]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, q->arr[j]);  /* { dg-warning "42" } */
}

/* "ptr" write: concrete region, but at symbolic offset.  */

struct st
test_conc_sym_ptr_conc_conc_arr (int i)
{
  struct st s;
  s.ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  s.arr[5] = 42;
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, s.arr[5]);  /* { dg-warning "42" } */
  return s;
}

struct st
test_conc_sym_ptr_conc_sym_arr (int i, int j)
{
  struct st s;
  s.ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  s.arr[j] = 42;
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, s.arr[j]);  /* { dg-warning "42" } */
  return s;
}

struct st
test_conc_sym_ptr_sym_conc_arr (int i, struct st *p)
{
  struct st s;
  s.ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  p->arr[5] = 42;
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  __analyzer_describe (0, p->arr[5]);  /* { dg-warning "42" } */
  return s;
} /* { dg-bogus "leak" "PR analyzer/105190" { xfail *-*-* } } */

struct st
test_conc_sym_ptr_sym_sym_arr (int i, struct st *p, int j)
{
  struct st s;
  s.ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  p->arr[j] = 42;
  __analyzer_describe (0, s.ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  __analyzer_describe (0, p->arr[j]);  /* { dg-warning "42" } */
  return s;
} /* { dg-bogus "leak" "PR analyzer/105190" { xfail *-*-* } } */

/* "ptr" write: symbolic region, with symbolic offset.  */

void
test_sym_sym_ptr_conc_conc_arr (struct st *p, int i)
{
  p->ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  g.arr[5] = 42;
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, g.arr[5]);  /* { dg-warning "42" } */
}

void
test_sym_sym_ptr_conc_sym_arr (struct st *p, int i, int j)
{
  p->ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  g.arr[j] = 42;
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, g.arr[j]);  /* { dg-warning "42" } */
}

void
test_sym_sym_ptr_sym_conc_arr (struct st *p, int i, struct st *q)
{
  p->ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  q->arr[5] = 42;
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, q->arr[5]);  /* { dg-warning "42" } */
}

void
test_sym_sym_ptr_sym_sym_arr (struct st *p, int i, struct st *q, int j)
{
  p->ptr[i] = __builtin_malloc (1024);
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "HEAP_ALLOCATED_REGION" } */
  q->arr[j] = 42;
  __analyzer_describe (0, p->ptr[i]); /* { dg-warning "UNKNOWN" } */
  __analyzer_describe (0, q->arr[j]);  /* { dg-warning "42" } */
}
