#include "../../gcc.dg/analyzer/analyzer-decls.h"

int test ()
{
  void *ptr = __builtin_malloc (1024); // { dg-message "allocated here" }

  try
    {
      throw 42;
    }
  catch (int i) // { dg-message "\.\.\.catching exception of type 'int' here" }
    {
      return -1;
    } // { dg-warning "leak of 'ptr'" }

  __builtin_free  (ptr);
  return 0;
}
