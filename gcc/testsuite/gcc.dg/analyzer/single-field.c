#include <stdlib.h>
#include "analyzer-decls.h"

struct foo
{
  char *ptr;
};

void test_1 (struct foo f)
{
  __analyzer_describe (0, f.ptr); /* { dg-warning "svalue: 'INIT_VAL\\(f.ptr\\)'" } */
}

static void __analyzer_called_by_test_2 (struct foo f_inner)
{
  free (f_inner.ptr);
  free (f_inner.ptr); /* { dg-warning "double-'free' of 'f_outer.ptr'" } */
}
void test_2 (struct foo f_outer)
{
  __analyzer_called_by_test_2 (f_outer);
}

struct nested
{
  struct foo f;
};

static void __analyzer_called_by_test_3 (struct nested n_inner)
{
  free (n_inner.f.ptr);
  free (n_inner.f.ptr); /* { dg-warning "double-'free' of 'n_outer.f.ptr'" } */
}
void test_3 (struct nested n_outer)
{
  __analyzer_called_by_test_3 (n_outer);
}
