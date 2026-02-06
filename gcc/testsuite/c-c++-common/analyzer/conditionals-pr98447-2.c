#include "analyzer-decls.h"

void test_1 (int i)
{
  __analyzer_eval (64 <= (i % 64)); /* { dg-warning "FALSE" } */
  __analyzer_eval (64 < (i % 64)); /* { dg-warning "FALSE" } */
  __analyzer_eval (63 <= (i % 64)); /* { dg-warning "UNKNOWN" } */

  /* (i % 64) > X for various X.  */
  __analyzer_eval ((i % 64) > -65); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i % 64) > -64); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i % 64) > -63); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) > 62); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) > 63); /* { dg-warning "FALSE" } */
  __analyzer_eval ((i % 64) > 64); /* { dg-warning "FALSE" } */

  /* (i % 64) >= X for various X.  */
  __analyzer_eval ((i % 64) >= -64); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i % 64) >= -63); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i % 64) >= -62); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) >= 62); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) >= 63); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) >= 64); /* { dg-warning "FALSE" } */

  /* (i % 64) < X for various X.  */
  __analyzer_eval ((i % 64) < -64); /* { dg-warning "FALSE" } */
  __analyzer_eval ((i % 64) < -63); /* { dg-warning "FALSE" } */
  __analyzer_eval ((i % 64) < -62); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) < 63); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) < 64); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i % 64) < 65); /* { dg-warning "TRUE" } */

  /* (i % 64) <= X for various X.  */
  __analyzer_eval ((i % 64) <= -64); /* { dg-warning "FALSE" } */
  __analyzer_eval ((i % 64) <= -63); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) <= -62); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) <= 62); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval ((i % 64) <= 63); /* { dg-warning "TRUE" } */
  __analyzer_eval ((i % 64) <= 64); /* { dg-warning "TRUE" } */
}
