/* { dg-additional-options "-fno-analyzer-suppress-followups" } */

extern void pipe(int pipefd[2]);
extern int close(int fd);

void
test_unchecked (void)
{
  int fds[2];
  pipe (fds); /* { dg-message "when 'pipe' fails" } */
  close (fds[0]); /* { dg-warning "use of uninitialized value 'fds\\\[0\\\]'" } */
  close (fds[1]); /* { dg-warning "use of uninitialized value 'fds\\\[1\\\]'" } */
}
