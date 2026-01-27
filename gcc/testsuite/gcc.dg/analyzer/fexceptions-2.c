/* Verify that -fanalyzer-assume-nothrow suppresses warnings about
   exceptions being thrown in called function, even those not
   marked with "nothrow".  */

/* { dg-additional-options "-fexceptions -fanalyzer-assume-nothrow" } */

extern void do_something ();
extern void do_something_nothrow () __attribute__ ((nothrow));

void test ()
{
  void *p = __builtin_malloc (1024);

  do_something (); /* { dg-bogus "leak of 'p'" } */

  __builtin_free (p);
}

void test_nothrow ()
{
  void *p = __builtin_malloc (1024);

  do_something_nothrow (); /* { dg-bogus "leak of 'p'" } */

  __builtin_free (p);
}
