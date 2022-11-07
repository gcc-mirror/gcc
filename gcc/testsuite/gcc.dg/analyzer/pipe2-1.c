#include "analyzer-decls.h"

extern int pipe2(int pipefd[2], int flags);
extern int close(int fd);

void
test_leak (void)
{
  int fds[2];
  if (pipe2 (fds, 0) == -1) /* { dg-message "when 'pipe2' succeeds" } */
    /* { dg-message "opened here as read-write" "sm msg" { target *-*-* } .-1 }} */
    return;
} /* { dg-line leak } */
/* { dg-warning "leak of file descriptor 'fds\\\[0\\\]'" "leak of 0" { target *-*-* } leak } */
/* { dg-warning "leak of file descriptor 'fds\\\[1\\\]'" "leak of 1" { target *-*-* } leak } */
/* { dg-message "'fds\\\[0\\\]' leaks here" "final msg 0" { target *-*-* } leak }} */
/* { dg-message "'fds\\\[1\\\]' leaks here" "final msg 1" { target *-*-* } leak }} */

void
test_close (void)
{
  int fds[2];
  if (pipe2 (fds, 0) == -1)
    return;
  __analyzer_describe (0, fds[0]); /* { dg-warning "CONJURED" } */
  __analyzer_describe (0, fds[1]); /* { dg-warning "CONJURED" } */
  close (fds[0]);
  close (fds[1]);
}

void
test_unchecked (void)
{
  int fds[2];
  pipe2 (fds, 0); /* { dg-message "when 'pipe2' fails" } */
  close (fds[0]); /* { dg-warning "use of uninitialized value 'fds\\\[0\\\]'" } */
  close (fds[1]); /* { dg-warning "use of uninitialized value 'fds\\\[1\\\]'" } */
}
