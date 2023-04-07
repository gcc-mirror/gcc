/* Adapted from gcc.dg/diagnostic-input-charset-1.c  */
/* { dg-do compile } */
/* { dg-require-iconv "CP850" } */
/* { dg-options "-finput-charset=CP850 -fdiagnostics-format=sarif-file" } */
/* { dg-excess-errors "The error is sent to the SARIF file, rather than stderr" } */

/* Test that diagnostics are converted to UTF-8; this file is encoded in
   CP850.

   The non-ASCII byte here is 0xf5, which when decoded as CP850
   is U+00A7 SECTION SIGN  */
const char *section = "õ"

/* 
   { dg-final { verify-sarif-file } }

   Verify that we captured the expected warning, and converted the snippet to
   UTF-8 on output.

   { dg-final { scan-sarif-file {"text": "expected ',' or ';' at end of input"} } }
   { dg-final { scan-sarif-file {"text": "const char .section = \\"\u00a7\\"} } }
*/
