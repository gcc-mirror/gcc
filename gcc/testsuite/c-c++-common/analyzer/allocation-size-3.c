/* { dg-additional-options "-fanalyzer-fine-grained" }
   -fanalyzer-fine-grained is currently required; see PR analyzer/107851.  */

/* { dg-additional-options -Wno-analyzer-out-of-bounds } */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/* CWE-131 example 5 */
void test_1 (void) 
{
  int32_t *id_sequence = (int32_t *) malloc (3); /* { dg-line malloc1 } */
  if (id_sequence == NULL) exit (1);

  id_sequence[0] = 13579;
  id_sequence[1] = 24680;
  id_sequence[2] = 97531;

  free (id_sequence);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc1 } */
  /* { dg-message "3 bytes" "note" { target *-*-* } malloc1 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } malloc1 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } malloc1 } */
}

void test_2 (void)
{
  int32_t *ptr = (int32_t *) malloc (10 + sizeof(int32_t)); /* { dg-line malloc2 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc2 } */
  /* { dg-message "14 bytes" "note" { target *-*-* } malloc2 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } malloc2 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } malloc2 } */
}

void test_3 (int32_t n)
{
  int32_t *ptr = (int32_t *) malloc (n + sizeof (int32_t)); /* { dg-line malloc3 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc3 } */
  /* { dg-message "'\[a-z0-9\\+\\(\\)\\s\]*' bytes" "note" { target *-*-* } malloc3 } */
  /* { dg-message "'int32_t \\*' (\\\{aka '(long )?int \\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c } malloc3 } */
  /* { dg-message "'int32_t\\*' (\\\{aka '(long )?int\\*'\\\})? here; 'sizeof \\(int32_t (\\\{aka (long )?int\\\})?\\)' is '4'" "note" { target c++ } malloc3 } */
}

void test_4 (int32_t n, int32_t m)
{
  int32_t *ptr = (int32_t *) malloc ((n + m) * sizeof (int32_t));
  free (ptr);
}
