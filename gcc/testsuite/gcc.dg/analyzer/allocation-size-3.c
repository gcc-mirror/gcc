#include <stdlib.h>
#include <stdio.h>

/* CWE-131 example 5 */
void test_1 (void) 
{
  int *id_sequence = (int *) malloc (3); /* { dg-line malloc1 } */
  if (id_sequence == NULL) exit (1);

  id_sequence[0] = 13579;
  id_sequence[1] = 24680;
  id_sequence[2] = 97531;

  free (id_sequence);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc1 } */
  /* { dg-message "\\d+ bytes" "note" { target *-*-* } malloc1 } */
  /* { dg-message "'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } malloc1 } */
}

void test_2 (void)
{
  int *ptr = malloc (10 + sizeof(int)); /* { dg-line malloc2 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc2 } */
  /* { dg-message "\\d+ bytes" "note" { target *-*-* } malloc2 } */
  /* { dg-message "'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } malloc2 } */
}

void test_3 (int n)
{
  int *ptr = malloc (n + sizeof (int)); /* { dg-line malloc3 } */
  free (ptr);

  /* { dg-warning "allocated buffer size is not a multiple of the pointee's size \\\[CWE-131\\\]" "warning" { target *-*-* } malloc3 } */
  /* { dg-message "'\[a-z0-9\\+\\(\\)\\s\]*' bytes" "note" { target *-*-* } malloc3 } */
  /* { dg-message "'int \\*' here; 'sizeof \\(int\\)' is '\\d+'" "note" { target *-*-* } malloc3 } */
}

void test_4 (int n, int m)
{
  int *ptr = malloc ((n + m) * sizeof (int));
  free (ptr);
}
