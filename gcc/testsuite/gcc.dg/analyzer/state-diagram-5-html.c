/* { dg-require-dot "" } */
/* { dg-additional-options "-fdiagnostics-add-output=experimental-html:javascript=no,show-state-diagrams=yes" } */
/* { dg-additional-options "-fdiagnostics-show-caret" } */

#include "analyzer-decls.h"

struct foo
{
  int m_ints[4];
};

struct bar
{
  struct foo m_foos[3];
  int m_int;
  char m_ch;
};

struct baz
{
  struct bar m_bars[2];
  struct foo m_foos[5];
};

void test (void)
{
  struct baz baz_arr[2];
  baz_arr[1].m_bars[1].m_foos[2].m_ints[1] = 42;
  __analyzer_dump_path (); /* { dg-message "path" } */
}

/* We need -fdiagnostics-show-caret for the HTML output to show the source,
   and thus to show event labels.  */

/* { dg-begin-multiline-output "" }
  __analyzer_dump_path ();
   { dg-end-multiline-output "" } */

/* Use a Python script to verify various properties about the generated
   .html file:
   { dg-final { run-html-pytest state-diagram-5-html.c "state-diagram-5-html.py" } } */
