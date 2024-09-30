/* { dg-do compile } */
/* { dg-additional-options " -fno-diagnostics-json-formatting -fdiagnostics-format=sarif-file" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

void test_1 (void)
{
  void *ptr = malloc (1024);
#include "sarif-path-role.h"
  free (ptr);
}

/* Verify SARIF output.

     { dg-final { verify-sarif-file } }

   Verify that the artifact for the header has this role, given
   that it's only referenced by an execution path event.

     { dg-final { scan-sarif-file "\"roles\": \\\[\"tracedFile\"\\\]" } }
*/
