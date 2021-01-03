/* OpenACC 'cache' directive: valid usage.  */

/* See also corresponding C/C++ variant '../../c-c++-common/goacc/cache-1.c'.  */

/* For execution testing, this file is '#include'd from
   '../../../../libgomp/testsuite/libgomp.oacc-c++/cache-1.C'.  */

#define TEMPLATIZE
#include "../../c-c++-common/goacc/cache-1.c"

static void
instantiate ()
{
  &test<0>;
}
