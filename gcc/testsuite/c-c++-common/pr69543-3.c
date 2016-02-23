/* { dg-options "-Wuninitialized" } */

/* Verify disabling a warning, where the _Pragma is in regular code,
   but the affected code is within a macro.  */

/* TODO: XFAIL: both C and C++ erroneously fail to suppress the warning
   The warning is reported at the macro definition location, rather than
   the macro expansion location.  */

#define WARNABLE_CODE *++yyvsp = yylval; /* { dg-bogus "used uninitialized" "" { xfail *-*-* } } */

void test (char yylval)
{
  char *yyvsp; /* { dg-bogus "declared here" "" { xfail *-*-* } } */
  _Pragma ("GCC diagnostic push")
  _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
  _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
  WARNABLE_CODE
  _Pragma ("GCC diagnostic pop")
}
