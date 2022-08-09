/* Verify that we check for uninitialized values passed to functions
   that we have special-cased region-model handling for.  */

extern void error (int __status, int __errnum, const char *__format, ...)
     __attribute__ ((__format__ (__printf__, 3, 4)));

void test_uninit_status (int arg)
{
  int st;
  error (st, 42, "test: %s", arg); /* { dg-warning "use of uninitialized value 'st'" } */
}

void test_uninit_errnum (int st)
{
  int num;
  error (st, num, "test"); /* { dg-warning "use of uninitialized value 'num'" } */
}

void test_uninit_fmt (int st)
{
  const char *fmt;
  error (st, 42, fmt); /* { dg-warning "use of uninitialized value 'fmt'" } */
}

void test_uninit_vargs (int st)
{
  int arg;
  error (st, 42, "test: %s", arg); /* { dg-warning "use of uninitialized value 'arg'" } */
}
