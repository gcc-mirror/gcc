/* { dg-options "-Wuninitialized" } */

/* Verify disabling a warning, where both the _Pragma and the
   affected code are *not* in a macro.  */

void test (char yylval)
{
  char *yyvsp;
  _Pragma ("GCC diagnostic push")
  _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
  _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
  *++yyvsp = yylval;
  _Pragma ("GCC diagnostic pop")
}
