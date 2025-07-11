/* { dg-additional-options "-fdiagnostics-add-output=sarif:state-graphs=yes" } */

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

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest state-diagram-5-sarif.c "state-diagram-5-sarif.py" } } */
