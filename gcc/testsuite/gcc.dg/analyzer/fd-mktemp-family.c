/* { dg-skip-if "fcntl,unistd" { avr-*-* } } */

/* Tests for fd leak and errno handling of mktemp-family functions.  */
/* { dg-additional-options "-Wno-analyzer-null-argument" } */

#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

extern int mkostemp (char *, int);
extern int mkostemps (char *, int, int);

/* For the following 8 tests, leak warnings may be reported at the call or at
   the closing brace, perhaps depending on system headers.  Keep each body on
   one line so the dg-warning directive matches either location.  */

void test_mkstemp_leak_no_lhs (char *s)
{ mkstemp (s); } /* { dg-warning "leak of file descriptor" } */

void test_mkostemp_leak_no_lhs (char *s, int flags)
{ mkostemp (s, flags); } /* { dg-warning "leak of file descriptor" } */

void test_mkstemps_leak_no_lhs (char *s, int suffixlen)
{ mkstemps (s, suffixlen); } /* { dg-warning "leak of file descriptor" } */

void test_mkostemps_leak_no_lhs (char *s, int suffixlen, int flags)
{ mkostemps (s, suffixlen, flags); } /* { dg-warning "leak of file descriptor" } */

void test_mkstemp_leak_unchecked (char *s)
{ int fd = mkstemp (s); } /* { dg-warning "leak of file descriptor 'fd'" } */

void test_mkostemp_leak_unchecked (char *s, int flags)
{ int fd = mkostemp (s, flags); } /* { dg-warning "leak of file descriptor 'fd'" } */

void test_mkstemps_leak_unchecked (char *s, int suffixlen)
{ int fd = mkstemps (s, suffixlen); } /* { dg-warning "leak of file descriptor 'fd'" } */

void test_mkostemps_leak_unchecked (char *s, int suffixlen, int flags)
{ int fd = mkostemps (s, suffixlen, flags); } /* { dg-warning "leak of file descriptor 'fd'" } */

void test_mkstemp_leak_checked (char *s)
{
  int fd = mkstemp (s); /* { dg-message "opened here" } */
  if (fd == -1)
    return;
} /* { dg-warning "leak of file descriptor 'fd'" } */

void test_mkostemp_leak_checked (char *s, int flags)
{
  int fd = mkostemp (s, flags); /* { dg-message "opened here" } */
  if (fd == -1)
    return;
} /* { dg-warning "leak of file descriptor 'fd'" } */

void test_mkstemps_leak_checked (char *s, int suffixlen)
{
  int fd = mkstemps (s, suffixlen); /* { dg-message "opened here" } */
  if (fd == -1)
    return;
} /* { dg-warning "leak of file descriptor 'fd'" } */

void test_mkostemps_leak_checked (char *s, int suffixlen, int flags)
{
  int fd = mkostemps (s, suffixlen, flags); /* { dg-message "opened here" } */
  if (fd == -1)
    return;
} /* { dg-warning "leak of file descriptor 'fd'" } */

/* TODO: test for 'close' on unchecked mkstemp-family fd.  */

void test_mkstemp_errno (char *s)
{
  errno = 0;
  int fd = mkstemp (s);
  if (fd == -1)
    {
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      return;
    }
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  close (fd);
}

void test_mkostemp_errno (char *s, int flags)
{
  errno = 0;
  int fd = mkostemp (s, flags);
  if (fd == -1)
    {
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      return;
    }
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  close (fd);
}

void test_mkstemps_errno (char *s, int suffixlen)
{
  errno = 0;
  int fd = mkstemps (s, suffixlen);
  if (fd == -1)
    {
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      return;
    }
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  close (fd);
}

void test_mkostemps_errno (char *s, int suffixlen, int flags)
{
  errno = 0;
  int fd = mkostemps (s, suffixlen, flags);
  if (fd == -1)
    {
      __analyzer_eval (errno > 0); /* { dg-warning "TRUE" } */
      return;
    }
  __analyzer_eval (errno == 0); /* { dg-warning "TRUE" } */
  close (fd);
}
