#include "analyzer-decls.h"

extern int errno;

extern void error (int __status, int __errnum, const char *__format, ...)
     __attribute__ ((__format__ (__printf__, 3, 4)));

extern void error_at_line (int __status, int __errnum, const char *__fname,
			   unsigned int __lineno, const char *__format, ...)
     __attribute__ ((__format__ (__printf__, 5, 6)));

/* When status is an unknown param.  */

void test_1 (int st)
{
  error (st, errno, "test");
  __analyzer_eval (st == 0); /* { dg-warning "TRUE" } */
}

/* When status is known zero.  */

void test_2 (int st)
{
  error (0, errno, "test");
  __analyzer_dump_path (); /* { dg-message "here" } */
}

/* When status is a non-zero known constant.  */

void test_3 (int st)
{
  error (1, errno, "test");
  __analyzer_dump_path (); /* { dg-bogus "here" } */
}

/* When status has been tested against zero.  */

void test_4 (int st)
{
  if (st)
    {
      error (st, errno, "nonzero branch");
      __analyzer_dump_path (); /* { dg-bogus "here" } */
    }
  else
    {
      error (st, errno, "zero branch");
      __analyzer_dump_path (); /* { dg-message "here" } */
    }
}

/* Similarly for error_at_line.  */

void test_5 (int st)
{
  error_at_line (st, errno, __FILE__, __LINE__, "test");
  __analyzer_eval (st == 0); /* { dg-warning "TRUE" } */
}

/* Non-trivial format string.  */

void test_6 (int st, const char *str)
{
  error (st, errno, "test: %s", str);
  __analyzer_eval (st == 0); /* { dg-warning "TRUE" } */
}
