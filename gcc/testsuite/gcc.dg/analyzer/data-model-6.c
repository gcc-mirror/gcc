#include <stdlib.h>
#include "analyzer-decls.h"

/* Verify that we don't accumulate state after a malloc/free pair.  */

void test (void)
{
  void *ptr;
  __analyzer_dump_num_heap_regions (); /* { dg-warning "num heap regions: '0'" } */
  ptr = malloc (1024);
  __analyzer_dump_num_heap_regions (); /* { dg-warning "num heap regions: '1'" } */
  free (ptr);
  __analyzer_dump_num_heap_regions (); /* { dg-warning "num heap regions: '0'" } */
}
