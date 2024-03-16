#include "analyzer-decls.h"

__attribute__ ((tainted_args))
double pr110700 (double x, double y)
{
  /* Ideally we'd complain here with -Wanalyzer-tainted-divisor, but
     until we track conditions on floating point values, we can't check to
     see if they've been checked against zero.  */
  return x / y;
}
