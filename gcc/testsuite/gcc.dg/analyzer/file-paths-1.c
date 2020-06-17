/* { dg-additional-options "-fanalyzer-verbosity=3" } */

#include <stdio.h>

/* Verify that we correctly emit CFG events in the face of buffers
   being clobbered in these leak reports.  */

void f1 (const char *str)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
  char buf[10];

  while (fgets(buf, 10, fp) != NULL) /* { dg-message "following 'false' branch\\.\\.\\." } */
    {
    }
} /* { dg-warning "leak of FILE 'fp'" "warning" } */
/* { dg-message "\\.\\.\\.to here" "to here" { target *-*-* } .-1 } */

void f2(const char *str, int flag)
{
  FILE * fp = fopen(str, "r"); /* { dg-message "opened here" } */
  char buf[10];

  if (flag) /* { dg-message "when 'flag == 0'" } */
    fclose(fp);
} /* { dg-warning "leak of FILE 'fp'" "warning" } */
/* { dg-message "\\.\\.\\.to here" "to here" { target *-*-* } .-1 } */
