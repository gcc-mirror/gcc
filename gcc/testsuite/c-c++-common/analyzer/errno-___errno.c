#include "analyzer-decls.h"

/* According to PR 107807 comment #2, Solaris implements "errno"
   like this:  */

extern int *___errno(void) __attribute__((__const__));
#define errno (*(___errno()))


extern void external_fn (void);

int test_reading_errno (void)
{
  return errno;
}

void test_setting_errno (int val)
{
  errno = val;
}

void test_storing_to_errno (int val)
{
  __analyzer_eval (errno == val); /* { dg-warning "UNKNOWN" } */
  errno = val;
  __analyzer_eval (errno == val); /* { dg-warning "TRUE" } */
  external_fn ();
  __analyzer_eval (errno == val); /* { dg-warning "UNKNOWN" } */
}
