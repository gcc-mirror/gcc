/* { dg-additional-options "-fno-analyzer-transitivity" } */

#include "analyzer-decls.h"

int foo (void);

/* Infeasible path, requiring transitivity to find.  */

void test_1 (int co, int y)
{
  if (4 < co)
    if (co < y)
      if (y == 0)
	__analyzer_dump_path (); /* { dg-message "path" } */
}

/* Infeasible path, requiring transitivity to find, with a merger.  */

void test_2 (int co, int y, int z)
{
  if (4 < co)
    if (co < y)
      if (y == 0)
	{
	  while (foo ())
	    {
	    }
	  __analyzer_dump_path (); /* { dg-message "path" } */
	}
}
