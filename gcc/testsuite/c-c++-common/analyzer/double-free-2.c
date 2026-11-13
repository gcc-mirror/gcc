extern void do_stuff (void);

void test (void *p)
{
  __builtin_free (p); /* { dg-message "first 'free' here" } */
  do_stuff (); 
  __builtin_free (p); /* { dg-warning "double-'free' of 'p'" } */
}
