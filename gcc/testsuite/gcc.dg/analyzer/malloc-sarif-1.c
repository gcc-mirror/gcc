/* { dg-do compile } */
/* { dg-additional-options "-fdiagnostics-format=sarif-file" } */

#include <stdlib.h>

void test_1 (void)
{
  void *ptr = malloc (1024);
  free (ptr);
  free (ptr);
}

/* Verify SARIF output.

   The threadFlowLocation objects should have "kinds" properties
   reflecting the meanings of the events:
     { dg-final { scan-sarif-file "\"kinds\": \\\[\"acquire\", \"memory\"\\\]" } }
     { dg-final { scan-sarif-file "\"kinds\": \\\[\"release\", \"memory\"\\\]" } }
     { dg-final { scan-sarif-file "\"kinds\": \\\[\"danger\"\\\]" } }
*/
