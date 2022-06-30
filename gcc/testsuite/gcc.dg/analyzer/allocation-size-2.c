#include <stdlib.h>
#include <stdio.h>

/* Tests with symbolic buffer sizes.  */

void test_1 (int n)
{
  short *ptr = malloc (n * sizeof (short));
  free (ptr);
}

void test_2 (int n)
{
  int *ptr = malloc (n * sizeof (short)); /* { dg-line malloc2 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc2 } */
  /* { dg-message "'\[a-z0-9\\*\\(\\)\\s\]*' bytes" "note" { target *-*-* } malloc2 } */
  /* { dg-message "'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } malloc2 } */
}

void test_3 (int n)
{
  void *ptr = malloc (n * sizeof (short));
  short *sptr = (short *)ptr;
  free (sptr);
}

void test_4 (int n)
{
  void *ptr = malloc (n * sizeof (short)); /* { dg-message "'\[a-z0-9\\*\\(\\)\\s\]*'" "note" } */
  int *iptr = (int *)ptr; /* { dg-line assign4 } */
  free (iptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign4 } */
  /* { dg-message "'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } assign4 } */
}

void test_5 (void)
{
  int user_input;
  scanf("%i", &user_input);
  int n;
  if (user_input == 0)
    n = 3 * user_input * sizeof (short);
  else
    n = 5 * user_input * sizeof (short);
  void *ptr = malloc (n);
  short *sptr = (short *)ptr;
  free (sptr);
}

void test_6 (void)
{
  int user_input;
  scanf("%i", &user_input);
  int n;
  if (user_input == 0)
    n = user_input;
  else if (user_input == 2)
    n = user_input * 3;
  else
    n = user_input * 5;
  /* n is an unknown_svalue at this point.  */
  void *ptr = malloc (n);
  int *iptr = (int *)ptr;
  free (iptr);
}

void *create_buffer(int n)
{
  return malloc(n);
}

void test_7(int n) 
{
  int *buf = create_buffer(n * sizeof (int));
  free (buf);
}

void test_8(int n) 
{
  /* FIXME: At the moment, region_model::set_value (lhs, <return_value>)
     is called at the src_node of the return edge. This edge has no stmts
     associated with it, leading to a rejection of the warning inside
     impl_region_model_context::warn. To ensure that the indentation
     in the diagnostic is right, the warning has to be emitted on an EN
     that is after the return edge.  */
  int *buf = create_buffer(n * sizeof(short)); /* { dg-warning "" "" { xfail *-*-* } } */
  free (buf);
}

void test_9 (void)
{
  int n;
  scanf("%i", &n);
  /* n is a conjured_svalue.  */
  void *ptr = malloc (n); /* { dg-message "'n' bytes" "note" } */
  int *iptr = (int *)ptr; /* { dg-line assign9 } */
  free (iptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign9 } */
  /* { dg-message "'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } assign9 } */
}

void test_11 (void)
{
  int n;
  scanf("%i", &n);
  void *ptr = malloc (n);
  if (n == sizeof (int))
    {
      /* n is a conjured_svalue but guarded such that we
         know the value is a multiple of sizeof (*iptr).  */
      int *iptr = (int *)ptr;
      free (iptr);
    }
  else
    free (ptr);
}

void test_12 (void)
{
  int n;
  scanf("%i", &n);
  void *ptr = malloc (n); /* { dg-message "'n' bytes" } */
  if (n == 5)
    {
      /* n is a conjured_svalue but guarded such that we
         know the value isn't a multiple of sizeof (*iptr).  */
      int *iptr = (int *)ptr; /* { dg-line assign12 } */
      free (iptr);
    }
  else
    free (ptr);
  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } assign12 } */
  /* { dg-message "'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } assign12 } */
}

void test_13 (void)
{
  int n;
  scanf("%i", &n);
  void *ptr = malloc (n);
  if (n == n * n)
    {
      /* n is a conjured_svalue but guarded such that we don't have an
         equivalence class for it. In such cases, we assume that the
         condition ensures that the value is okay.  */
      int *iptr = (int *)ptr;
      free (iptr);
    }
  else
    free (ptr);
}
