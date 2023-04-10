/* Try to process this explicitly as UTF-8.  */

/* { dg-do compile } */
/* { dg-options "-finput-charset=utf-8 -fdiagnostics-format=sarif-file" } */
/* { dg-excess-errors "The error is sent to the SARIF file, rather than stderr" } */

const char *section = "þ"

/* The above in quotes is byte 0xFE which is not valid in UTF-8.
   Verify that we can generate a valid UTF-8 .sarif file complaining
   about the missing semicolon above.  */

/* { dg-final { verify-sarif-file } }

     { dg-final { scan-sarif-file {"text": "expected ',' or ';' at end of input"} } }
*/
