/* Check that we push the declaration and then continue translation.  */
/* { dg-do compile }
   { dg-require-iconv "IBM1047" }
   { dg-final { scan-assembler-not "foobar" } } */
extern "C" { char *foo = "foobar"; }
