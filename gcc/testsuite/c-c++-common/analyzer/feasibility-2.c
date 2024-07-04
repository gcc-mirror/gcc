/* Verify that -fno-analyzer-feasibility works.  */
/* { dg-additional-options "-fno-analyzer-feasibility" } */

#include "analyzer-decls.h"

void test_1 (int flag)
{
  int a;
  if (flag)
    a = 1;
  else
    a = 2;

  if (a == 1) /* (can only be the case when "flag" was true above).  */
    if (!flag)
      {
	__analyzer_dump_path (); /* { dg-message "note: path" "path diag" } */
	/* { dg-message "infeasible" "infeasibility event" { target *-*-* } .-1 } */
      }
}
