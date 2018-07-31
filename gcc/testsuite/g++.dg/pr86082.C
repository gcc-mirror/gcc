/* { dg-do link } */
/* { dg-require-iconv "IBM1047" } */
/* { dg-options "-fexec-charset=IBM1047 -std=c++11" } */

/* When turning 123_test into an operator invocation the literal 123
   needs to be translated to the execution character set.  Failing to
   do so results in a link error since '1', '2', and '3' in the
   specialization will be translated as string literals.  */

template <char... p> void q();
template <> void q<'1','2','3'>() {}

template <char... p> void operator""_test() { q<p...> (); }

int
main ()
{
  123_test;
}
