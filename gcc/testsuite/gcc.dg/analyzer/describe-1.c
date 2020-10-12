/* Smoketest for __analyzer_describe.  */

#include "analyzer-decls.h"

void test (int i)
{
  __analyzer_describe (0, 42); /* { dg-warning "svalue: '\\(int\\)42'" } */
  __analyzer_describe (0, i); /* { dg-warning "svalue: 'INIT_VAL\\(i.*\\)'" } */
  __analyzer_describe (0, &i); /* { dg-warning "svalue: '&i'" } */
  /* Further cases would risk overspecifying things. */
}
