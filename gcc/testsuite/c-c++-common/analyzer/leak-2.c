#include <stdlib.h>
#include "../../gcc.dg/analyzer/analyzer-decls.h"

void *ptr;

void test (void)
{
  ptr = malloc (1024);
  ptr = NULL; /* { dg-warning "leak of 'ptr'" } */
}
