/* { dg-do compile } */
/* { dg-require-iconv "CP850" } */
/* { dg-options "-finput-charset=CP850 -fdiagnostics-show-caret" } */

/* Test that diagnostics are converted to UTF-8; this file is encoded in
   CP850.  Why CP850?  -finput-charset only supports encodings that are a
   superset of ASCII.  But encodings that look like latin-1 are automatically
   converted by expect to UTF-8, and hence by the time dg sees them, it can't
   verify they were actually output in UTF-8.  So codepage 850 was chosen as one
   that is hopefully available and meets the requirements of matching ASCII and
   not matching latin-1.  */
const char *section = "๕"
/* { dg-error "expected .* at end of input" "" { target *-*-*} .-1 } */
/* { dg-begin-multiline-output "" }
 const char *section = "ยง"
 ^~~~~
   { dg-end-multiline-output "" } */
