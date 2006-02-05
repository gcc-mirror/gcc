/* Simple character translation test.  */
/* { dg-do compile }
   { dg-require-iconv "IBM1047" }
   { dg-final { scan-assembler-not "string foobar" } } */
const char* foo = "string foobar";
