/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>
#include <stdint.h>

/* Tests related to structs.  */

struct base {
  int16_t i;
};

struct sub {
  struct base b;
  int16_t j;
};

struct var_len {
  int16_t i;
  char arr[];
};


void test_1 (void)
{
  struct base *ptr = (struct base *) malloc (5 * sizeof (struct base));
  free (ptr);
}

void test_2 (void)
{
  int32_t *ptr = (int32_t *) malloc (5 * sizeof (struct base));  /* { dg-line malloc2 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc2 } */
  /* { dg-message "\\d+ bytes" "note" { target *-*-* } malloc2 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } malloc2 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } malloc2 } */
}

void test_3 (void)
{
  /* Even though 10 bytes is not a multiple of 4, we do not warn to prevent
     a false positive in case s is the base struct of a struct inheritance.  */
  struct base *ptr = (struct base *) malloc (10);
  free (ptr);
}

void test_4 (void)
{
  struct var_len *ptr = (struct var_len *) malloc (10);
  free (ptr);
}

void test_5 (void)
{
  /* For constant sizes, we warn if the buffer
     is too small to hold a single struct.  */
  struct base *ptr = (struct base *) malloc (1);  /* { dg-line malloc5 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc5 } */
  /* { dg-message "allocated 1 byte here" "note" { target *-*-* } malloc5 } */
  /* { dg-message "'struct base \\*' here; 'sizeof \\(struct base\\)' is '\\d+'" "note" { target c } malloc5 } */
  /* { dg-message "'base\\*' here; 'sizeof \\(base\\)' is '\\d+'" "note" { target c++ } malloc5 } */
}
