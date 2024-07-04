/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

/* Generate a warning in a header file.  */
#include "diagnostic-format-sarif-file-header-role.h"

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Verify that the header file's "role" is "resultFile", as per "NOTE 3"
   in SARIF v2.1.0 section 3.24.6.  */
/* { dg-final { scan-sarif-file "\"roles\": \\\[\"resultFile\"\\\]" } } */
