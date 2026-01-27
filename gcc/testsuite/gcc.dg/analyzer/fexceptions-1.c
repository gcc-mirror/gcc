/* { dg-additional-options "-fexceptions" } */

extern void do_something ();
extern void do_something_nothrow () __attribute__ ((nothrow));

void test ()
{
  void *p = __builtin_malloc (1024);

  do_something (); /* { dg-warning "leak of 'p'" } */
  /* { dg-message "if 'do_something' throws an exception\.\.\." "exception event" { target *-*-* } .-1 } */

  __builtin_free (p);
}

void test_nothrow ()
{
  void *p = __builtin_malloc (1024);

  do_something_nothrow (); /* { dg-bogus "leak of 'p'" } */

  __builtin_free (p);
}
