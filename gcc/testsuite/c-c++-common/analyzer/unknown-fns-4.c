#include "analyzer-decls.h"

int get(void);
void test (void)
{
  int got = 0;
  while (1)
    {
      if (get ())
	got = 1;
      else
	if (got)
	  __analyzer_dump_path (); /* { dg-message "path" } */
    }
}
