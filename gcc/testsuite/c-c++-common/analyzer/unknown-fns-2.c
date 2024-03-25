/* Tests for data model handling of unknown fns.  */

#include <stddef.h>
#include "analyzer-decls.h"

void unknown_fn (void *);

void test_1 (void)
{
  int i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  unknown_fn (NULL);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */
  
  unknown_fn (&i);
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* Even though we're not passing &i to unknown_fn, it escaped
     above, so unknown_fn could write to it.  */
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "UNKNOWN" } */
}

/* As test_1, but with an unknown fn_ptr.  */

void test_1a (void (*fn_ptr) (void *))
{
  int i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  fn_ptr (NULL);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */
  
  fn_ptr (&i);
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* Even though we're not passing &i to unknown_fn, it escaped
     above, so fn_ptr (NULL) could write to it.  */
  fn_ptr (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "UNKNOWN" } */
}

int *global_for_test_2;

void test_2 (void)
{
  int i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  global_for_test_2 = &i;
  unknown_fn (NULL);
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */

  global_for_test_2 = NULL;

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* Even though the global no longer points to i, it escaped
     above, so unknown_fn could write to it.  */
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "UNKNOWN" } */
}

struct used_by_test_3
{
  int *int_ptr;
};

void test_3 (void)
{
  int i;

  struct used_by_test_3 s;
  s.int_ptr = &i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  unknown_fn (NULL);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (s.int_ptr == &i); /* { dg-warning "TRUE" } */

  /* i should escape here.  */
  unknown_fn (&s);
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (s.int_ptr == &i); /* { dg-warning "UNKNOWN" } */

  s.int_ptr = NULL;
  __analyzer_eval (s.int_ptr == NULL); /* { dg-warning "TRUE" } */

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* Even though nothing we know about points to i, it escaped
     above, so unknown_fn could write to it.  */
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "UNKNOWN" } */
}

struct used_by_test_4
{
  int *int_ptr;
};

void test_4 (struct used_by_test_4 *st4_ptr)
{
  /* Something unknown called "test_4", and hence *st4_ptr has
     effectively already escaped.  */

  int i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  unknown_fn (NULL);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  /* Given that *st4_ptr has effectively already escaped, calling
     an unknown fn should invalidate our knowledge of i".  */
  st4_ptr->int_ptr = &i;
  unknown_fn (NULL);
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */

  /* ...and "&i" should now be treated as having escaped.  */
  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */
  st4_ptr->int_ptr = NULL;
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "UNKNOWN" } */
}

static void __attribute__((noinline))
known_fn (void *ptr)
{
  /* Empty.  */
}

void test_5 (void)
{
  int i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  known_fn (&i);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* Ensure that we don't consider &i to have escaped.  */
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */
}

extern int __attribute__ ((__pure__))
unknown_pure_fn (void *);

void test_6 (void)
{
  int i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  unknown_pure_fn (&i);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* Ensure that we don't consider &i to have escaped.  */
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */
}

extern void unknown_const_fn (const void *);

void test_7 (void)
{
  int i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  /* &i is passed as a const void *, so i shouldn't be clobbered by
     the call.  */
  unknown_const_fn (&i);
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* Ensure that we don't consider &i to have escaped.  */
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */
}

struct used_by_test_8
{
  int *int_ptr;
};

void test_8 (void)
{
  int i;

  i = 42;
  __analyzer_eval (i == 42); /* { dg-warning "TRUE" } */

  struct used_by_test_8 st8;
  st8.int_ptr = &i;

  /* Although unknown_const_fn takes a const void *, the
     int_ptr is a non-const int *, and so &i should be considered
     writable.  */
  unknown_const_fn (&st8);
  __analyzer_eval (i == 42); /* { dg-warning "UNKNOWN" } */

  i = 17;
  __analyzer_eval (i == 17); /* { dg-warning "TRUE" } */

  /* &i should be considered to have escaped.  */
  unknown_fn (NULL);
  __analyzer_eval (i == 17); /* { dg-warning "UNKNOWN" } */
}
