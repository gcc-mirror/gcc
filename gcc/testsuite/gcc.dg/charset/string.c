/* Simple character translation test.  */
/* { dg-do compile }
   { dg-require-iconv "IBM1047" }
   { dg-final { scan-assembler-not "string foobar" } } */
char *foo = "string foobar";
