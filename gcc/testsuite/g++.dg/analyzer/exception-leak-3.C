extern void do_something ();

int test ()
{
  void *ptr = __builtin_malloc (1024); // { dg-message "allocated here" }

  do_something (); // { dg-message "if 'void do_something\\(\\)' throws an exception\.\.\." }
  // { dg-warning "leak of 'ptr'" "" { target *-*-* } .-1 }

  __builtin_free  (ptr);
  return 0;
}
