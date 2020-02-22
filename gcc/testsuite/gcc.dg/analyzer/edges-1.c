#include <stdio.h>

extern int foo ();
extern void bar ();

/* Verify that only significant edges are reported.  */

void test_1 (const char *path, int flag)
{
  FILE *fp = fopen (path, "r");

  if (!fp) /* { dg-message "when 'fp' is non-NULL" } */
    return;

  /* We shouldn't report this control flow.  */
  while (foo ()) /* { dg-bogus "" } */
    bar ();

  if (flag) /* { dg-message "when 'flag == 0'" "branch event" } */
    fclose (fp); /* { dg-bogus "leak" "warning at wrong location" { xfail *-*-* } .-1 } */
} /* { dg-warning "leak of FILE 'fp'" "warning" { xfail *-*-* } } */
// TODO(xfail): location of leak message ought to be on closing brace

void test_2 (const char *path, int flag)
{
  FILE *fp = fopen (path, "r");

  /* We shouldn't report this control flow.  */
  if (foo ()) /* { dg-bogus "" } */
    bar ();
  else
    bar ();

  if (flag) /* { dg-message "when 'flag == 0'" } */
    fclose (fp); 
} /* { dg-warning "leak of FILE 'fp'" } */

static void __attribute__((noinline))
called_by_test_3 (int flag)
{
  if (flag)
    foo ();
}

void test_3 (const char *path, int flag)
{
  FILE *fp = fopen (path, "r");

  /* We shouldn't report the call/return here.  */
  called_by_test_3 (flag); /* { dg-bogus "" } */

  if (flag) /* { dg-message "when 'flag == 0'" } */
    fclose (fp);
} /* { dg-warning "leak of FILE 'fp'" } */
