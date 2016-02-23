/* { dg-options "-Wuninitialized" } */

/* Verify disabling a warning, where the _Pragma is within
   a macro, but the affected code is *not* in a macro.  */

/* TODO: XFAIL: why does g++ still emit a warning here? (works for C).  */

# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN	\
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")

void test (char yylval)
{
  char *yyvsp;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval; /* { dg-bogus "used uninitialized" "" { xfail { c++ } } } */
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}
