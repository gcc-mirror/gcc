/* { dg-do compile } */
/* { dg-additional-options " -fno-diagnostics-json-formatting -fdiagnostics-format=sarif-file" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

void test_1 (void)
{
  void *ptr = malloc (1024);
  free (ptr);
  free (ptr);
}

/* Verify SARIF output.

     { dg-final { verify-sarif-file } }

   The threadFlowLocation objects should have "kinds" properties
   reflecting the meanings of the events:
     { dg-final { scan-sarif-file "\"kinds\": \\\[\"acquire\", \"memory\"\\\]" } }
     { dg-final { scan-sarif-file "\"kinds\": \\\[\"release\", \"memory\"\\\]" } }
     { dg-final { scan-sarif-file "\"kinds\": \\\[\"danger\"\\\]" } }
*/
