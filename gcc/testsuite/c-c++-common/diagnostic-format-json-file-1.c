/* Check that -fdiagnostics-format=json-file works.  */
/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=json-file" } */

#warning message

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { scan-file "diagnostic-format-json-file-1.c.gcc.json" "\"message\": \"#warning message\"" } } */
