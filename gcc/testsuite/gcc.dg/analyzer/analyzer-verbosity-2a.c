/* { dg-additional-options "-fanalyzer-verbosity=2" } */

#include <stdio.h>

extern int foo ();
extern void bar ();

void test (const char *path, int flag)
{
  FILE *fp = fopen (path, "r"); /* { dg-message "opened here" } */

  /* We shouldn't report this control flow at -fanalyzer-verbosity=2.  */
  if (foo ()) /* { dg-bogus "" } */
    bar ();
  else
    bar ();

  if (flag) /* { dg-message "when 'flag == 0'" } */
    fclose (fp); 
} /* { dg-warning "leak of FILE 'fp'" } */
