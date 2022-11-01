/* { dg-options "-Wuninitialized" } */

/* Verify disabling a warning, where the _Pragma is in regular code,
   but the affected code is within a macro.  */

#define WARNABLE_CODE *++yyvsp = yylval; /* { dg-bogus "used uninitialized" } */

void test (char yylval)
{
  char *yyvsp; /* { dg-bogus "declared here" } */
  _Pragma ("GCC diagnostic push")
  _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
  _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
  WARNABLE_CODE
  _Pragma ("GCC diagnostic pop")
}
