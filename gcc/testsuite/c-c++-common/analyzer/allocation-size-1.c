#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/* Tests with constant buffer sizes.  */

void test_1 (void)
{
  int16_t *ptr = (int16_t *) malloc (21 * sizeof (int16_t));
  free (ptr);
}

void test_2 (void)
{
  int32_t *ptr = (int32_t *) malloc (21 * sizeof (int16_t)); /* { dg-line malloc2 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc2 } */
  /* { dg-message "42 bytes" "note" { target *-*-* } malloc2 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } malloc2 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } malloc2 } */
}

void test_3 (void)
{
  void *ptr = malloc (21 * sizeof (int16_t));
  int16_t *sptr = (int16_t *)ptr;
  free (sptr);
}

void test_4 (void)
{
  void *ptr = malloc (21 * sizeof (int16_t)); /* { dg-message "42 bytes" } */
  int32_t *iptr = (int32_t *)ptr; /* { dg-line assign4 } */
  free (iptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign4 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } assign4 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } assign4 } */
}

void test_5 (void)
{
  int32_t user_input;
  scanf("%i", &user_input);
  int32_t n;
  if (user_input == 0)
    n = 21 * sizeof (int16_t);
  else
    n = 42 * sizeof (int16_t);
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
    n = 21 * sizeof (int16_t);
  else
    n = 42 * sizeof (int16_t);
  void *ptr = malloc (n); /* { dg-message "" "note" } */
                          /* ^^^ on widening_svalues no expr is returned
                                 by get_representative_tree at the moment.  */ 
  int32_t *iptr = (int32_t *)ptr; /* { dg-line assign6 } */
  free (iptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign6 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } assign6 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } assign6 } */
}

void test_7 (void)
{
  int32_t user_input;
  scanf("%i", &user_input);
  int32_t n;
  if (user_input == 0)
    n = 1;
  else if (user_input == 2)
    n = 5;
  else
    n = 7;
  /* n is an unknown_svalue at this point.  */
  void *ptr = malloc (n);
  int32_t *iptr = (int32_t *)ptr;
  free (iptr);
}

void *create_buffer (int32_t n)
{
  return malloc(n);
}

void test_8 (void) 
{
  int32_t *buf = (int32_t *) create_buffer(4 * sizeof (int));
  free (buf);
}

void test_9 (void) 
{
  /* FIXME: At the moment, region_model::set_value (lhs, <return_value>)
     is called at the src_node of the return edge. This edge has no stmts
     associated with it, leading to a rejection of the warning inside
     impl_region_model_context::warn. To ensure that the indentation
     in the diagnostic is right, the warning has to be emitted on an EN
     that is after the return edge.  */
  int32_t *buf = (int32_t *) create_buffer(42); /* { dg-warning "" "" { xfail *-*-* } } */
  free (buf);
}

void test_10 (int32_t n)
{
  char *ptr = (char *) malloc (7 * n);
  free (ptr);
}

void test_11 ()
{
  /* 3.0 is folded to an int before the analyzer runs.  */
  int32_t *ptr = (int32_t *) malloc (3.0); /* { dg-line malloc11 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc11 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } malloc11 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } malloc11 } */
}
