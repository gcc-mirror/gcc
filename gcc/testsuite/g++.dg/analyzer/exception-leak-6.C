#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern void do_something (int x);

int inner (int x)
{
  do_something (x); // { dg-warning "leak" }
  // { dg-message "if 'void do_something\\(int\\)' throws an exception\.\.\." "" { target *-*-* } .-1 }

  return 0;
}

int outer (int x)
{
  void *ptr = __builtin_malloc (1024); // { dg-message "allocated here" }

  int rval = inner (x);

  __builtin_free (ptr);

  return rval; 
}
