/*
  { dg-options "-fplugin-arg-location_overflow_plugin-value=0x60000001" }
  { dg-do preprocess }
*/

#include "location-overflow-test-pr83173.h"

int
main ()
{
  return 0;
}

/*
  The preprocessor output should contain
  '# 1 "path/to/location-overflow-test-pr83173-1.h" 1', but should not
  contain any other references to location-overflow-test-pr83183-1.h.

  { dg-final { scan-file location-overflow-test-pr83173.i "# 1 \[^\r\n\]+location-overflow-test-pr83173-1\.h\" 1" } }
  { dg-final { scan-file-not location-overflow-test-pr83173.i "# (?!1 \[^\r\n\]+location-overflow-test-pr83173-1\.h\" 1)\[0-9\]+ \[^\r\n\]+location-overflow-test-pr83173-1\.h\"" } }
*/
