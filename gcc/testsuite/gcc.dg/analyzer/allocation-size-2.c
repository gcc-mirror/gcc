/* { dg-additional-options "-fanalyzer-fine-grained" }
   -fanalyzer-fine-grained is currently required; see PR analyzer/107851.  */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/* Tests with symbolic buffer sizes.  */

void test_1 (int32_t n)
{
  int16_t *ptr = malloc (n * sizeof (int16_t));
  free (ptr);
}

void test_2 (int32_t n)
{
  int32_t *ptr = malloc (n * sizeof (int16_t)); /* { dg-line malloc2 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc2 } */
  /* { dg-message "'\[a-z0-9\\*\\(\\)\\s\]*' bytes" "note" { target *-*-* } malloc2 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4" "note" { target *-*-* } malloc2 } */
}

void test_3 (int32_t n)
{
  void *ptr = malloc (n * sizeof (int16_t));
  int16_t *sptr = (int16_t *)ptr;
  free (sptr);
}

void test_4 (int32_t n)
{
  void *ptr = malloc (n * sizeof (int16_t)); /* { dg-message "'\[a-z0-9\\*\\(\\)\\s\]*'" "note" } */
  int32_t *iptr = (int32_t *)ptr; /* { dg-line assign4 } */
  free (iptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign4 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target *-*-* } assign4 } */
}

void test_5 (void)
{
  int32_t user_input;
  scanf("%i", &user_input);
  int32_t n;
  if (user_input == 0)
    n = 3 * user_input * sizeof (int16_t);
  else
    n = 5 * user_input * sizeof (int16_t);
  void *ptr = malloc (n);
  int16_t *sptr = (int16_t *)ptr;
  free (sptr);
}

void test_6 (void)
{
  int32_t user_input;
  scanf("%i", &user_input);
  int32_t n;
  if (user_input == 0)
    n = user_input;
  else if (user_input == 2)
    n = user_input * 3;
  else
    n = user_input * 5;
  /* n is an unknown_svalue at this point.  */
  void *ptr = malloc (n);
  int32_t *iptr = (int32_t *)ptr;
  free (iptr);
}

void *create_buffer(int32_t n)
{
  return malloc(n);
}

void test_7(int32_t n) 
{
  int32_t *buf = create_buffer(n * sizeof (int32_t));
  free (buf);
}

void test_8(int32_t n) 
{
  /* FIXME: At the moment, region_model::set_value (lhs, <return_value>)
     is called at the src_node of the return edge. This edge has no stmts
     associated with it, leading to a rejection of the warning inside
     impl_region_model_context::warn. To ensure that the indentation
     in the diagnostic is right, the warning has to be emitted on an EN
     that is after the return edge.  */
  int32_t *buf = create_buffer(n * sizeof(int16_t)); /* { dg-warning "" "" { xfail *-*-* } } */
  free (buf);
}

void test_9 (void)
{
  int32_t n;
  scanf("%i", &n);
  /* n is a conjured_svalue.  */
  void *ptr = malloc (n); /* { dg-message "'n' bytes" "note" } */
  int32_t *iptr = (int32_t *)ptr; /* { dg-line assign9 } */
  free (iptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign9 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target *-*-* } assign9 } */
}

void test_11 (void)
{
  int32_t n;
  scanf("%i", &n);
  void *ptr = malloc (n);
  if (n == sizeof (int32_t))
    {
      /* n is a conjured_svalue but guarded such that we
         know the value is a multiple of sizeof (*iptr).  */
      int32_t *iptr = (int32_t *)ptr;
      free (iptr);
    }
  else
    free (ptr);
}

void test_12 (void)
{
  int32_t n;
  scanf("%i", &n);
  void *ptr = malloc (n); /* { dg-message "'n' bytes" } */
  if (n == 5)
    {
      /* n is a conjured_svalue but guarded such that we
         know the value isn't a multiple of sizeof (*iptr).  */
      int32_t *iptr = (int32_t *)ptr; /* { dg-line assign12 } */
      free (iptr);
    }
  else
    free (ptr);
  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign12 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target *-*-* } assign12 } */
}

void test_13 (void)
{
  int32_t n;
  scanf("%i", &n);
  void *ptr = malloc (n);
  if (n == n * n)
    {
      /* n is a conjured_svalue but guarded such that we don't have an
         equivalence class for it. In such cases, we assume that the
         condition ensures that the value is okay.  */
      int32_t *iptr = (int32_t *)ptr;
      free (iptr);
    }
  else
    free (ptr);
}
