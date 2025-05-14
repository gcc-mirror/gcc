#include "../../gcc.dg/analyzer/analyzer-decls.h"

int test ()
{
  void *ptr = __builtin_malloc (1024); // { dg-message "allocated here" }
  
  throw 42; // { dg-warning "leak of 'ptr'" }
}
