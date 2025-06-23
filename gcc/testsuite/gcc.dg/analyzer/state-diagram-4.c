#include "analyzer-decls.h"

void test ()
{
  void *p = __builtin_malloc (1024);
  __builtin_free (p);
  __builtin_free (p); /* { dg-warning "-Wanalyzer-double-free" } */
}
