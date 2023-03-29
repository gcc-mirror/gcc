/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <errno.h>
#include "analyzer-decls.h"

extern int isatty(int fd);
extern int close(int fd);

int test_pass_through (int fd)
{
  return isatty (fd);
}

void test_merging (int fd)
{
  isatty (fd);
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

int test_outcomes (int fd)
{
  errno = 0;
  int result = isatty (fd);
  switch (result)
    {
    default:
      __analyzer_dump_path (); /* { dg-bogus "path" } */
      break;
    case 0:
      __analyzer_dump_path (); /* { dg-message "path" } */
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      break;
    case 1:
      __analyzer_dump_path (); /* { dg-message "path" } */
      __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
      break;
    }
  return result;
}

int test_isatty_on_invalid_fd (void)
{
  errno = 0;
  int result = isatty (-1);
  __analyzer_eval (result == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
  return result;
}

int test_isatty_on_closed_fd (int fd)
{
  close (fd);
  errno = 0;
  int result = isatty (fd);
  __analyzer_eval (result == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
  return result;
}
