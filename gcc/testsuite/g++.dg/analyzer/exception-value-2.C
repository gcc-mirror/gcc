#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct foo {};

int inner (bool flag)
{
  try
    {
      if (flag)
	throw 42;
    }
  catch (foo &f)
    {
      return 1;
    }
  return 0;
}

int middle (bool flag)
{
  try
    {
      int rval = inner (flag);
      return rval;
    }
  catch (int ei)
    {
      return ei;
    }
}

void outer (void)
{
  __analyzer_eval (middle (false) == 0); // { dg-warning "TRUE" }
  __analyzer_eval (middle (true) == 42); // { dg-warning "TRUE" }
}
