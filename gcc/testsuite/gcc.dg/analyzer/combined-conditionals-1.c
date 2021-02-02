/* Verify that we correctly consolidate conditionals in paths.  */

#include "analyzer-decls.h"

extern int foo ();
extern int bar ();
extern int baz ();

void test_1 (int a, int b, int c)
{
  if (a && b && c) /* { dg-message "\\(1\\) following 'true' branch\\.\\.\\." } */
    __analyzer_dump_path (); /* { dg-message "\\(2\\) \\.\\.\\.to here" } */
}

void test_2 (int a, int b, int c)
{
  if (a && b) /* { dg-message "\\(1\\) following 'true' branch\\.\\.\\." } */
    if (c) /* { dg-message "\\(2\\) \\.\\.\\.to here" } */
      __analyzer_dump_path ();
}

void test_3 (int a, int b, int c)
{
  if (a) /* { dg-message "\\(1\\) following 'true' branch" } */
    if (b && c) /* { dg-message "\\(2\\) \\.\\.\\.to here" } */
      __analyzer_dump_path ();
}

void test_4 (void)
{
  while (foo () && bar ()) /* { dg-message "\\(1\\) following 'true' branch\\.\\.\\." } */
    __analyzer_dump_path (); /* { dg-message "\\(2\\) \\.\\.\\.to here" } */
}

void test_5 (int a, int b, int c)
{
  if (a || b || c) /* { dg-message "\\(1\\) following 'false' branch\\.\\.\\." } */
    {
    }
  else
    __analyzer_dump_path (); /* { dg-message "\\(2\\) \\.\\.\\.to here" } */
}

void test_6 (void)
{
  int i;
  for (i = 0; i < 10 && foo (); i++) /* { dg-message "\\(1\\) following 'true' branch\\.\\.\\." } */
    __analyzer_dump_path (); /* { dg-message "\\(2\\) \\.\\.\\.to here" } */
}

int test_7 (void)
{
  if (foo () ? bar () ? baz () : 0 : 0) /* { dg-message "\\(1\\) following 'true' branch\\.\\.\\." } */
    __analyzer_dump_path (); /* { dg-message "\\(2\\) \\.\\.\\.to here" } */    
}
