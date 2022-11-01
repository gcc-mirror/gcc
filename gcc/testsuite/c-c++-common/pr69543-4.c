/* { dg-options "-Wuninitialized" } */

/* Verify disabling a warning, where both the _Pragma and the
   affected code are within (different) macros.  */

# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN	\
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")

#define WARNABLE_CODE *++yyvsp = yylval; /* { dg-bogus "used uninitialized" } */

void test (char yylval)
{
  char *yyvsp; /* { dg-bogus "declared here" } */
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  WARNABLE_CODE
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}
