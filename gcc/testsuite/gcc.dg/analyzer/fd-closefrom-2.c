/* PR analyzer/106283 */
/* Test closefrom (int) with an int return type (NetBSD / OpenBSD).  */

#include "analyzer-decls.h"

extern int *__errno_location (void);
#define errno (*__errno_location ())

extern int accept (int fd, void *addr, unsigned *addrlen);
extern int bind (int fd, const void *addr, unsigned len);
extern int close (int fd);
extern int closefrom (int lowfd);
extern void free (void *);
extern int listen (int fd, int backlog);
extern void *malloc (__SIZE_TYPE__);
extern int open (const char *, int mode);
#define O_RDONLY 0

void
test_closefrom_int_success (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  int rc = closefrom (0);
  __analyzer_eval (rc == 0); /* { dg-warning "TRUE" } */
  close (fd); /* { dg-warning "double 'close' of file descriptor 'fd'" } */
}

int
test_closefrom_result (void)
{
  int result = closefrom (3);
  return result; /* { dg-bogus "use of uninitialized value" } */
}

int
test_closefrom_symbolic_result (int lowfd)
{
  int result = closefrom (lowfd);
  return result; /* { dg-bogus "use of uninitialized value" } */
}

void
test_closefrom_no_tracked_fds_symbolic_result (void)
{
  closefrom (3);
  int rc = closefrom (3);
  __analyzer_eval (rc == 0); /* { dg-warning "UNKNOWN" } */
}

void
test_closefrom_failed_bind_symbolic_result (int fd)
{
  if (fd < 3)
    return;
  char addr[16] = {0};
  if (bind (fd, addr, sizeof addr) == 0)
    {
      close (fd);
      return;
    }
  int rc = closefrom (fd);
  __analyzer_eval (rc == 0); /* { dg-warning "UNKNOWN" } */
  close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_failed_listen_symbolic_result (int fd)
{
  if (fd < 3)
    return;
  if (listen (fd, 1) == 0)
    {
      close (fd);
      return;
    }
  int rc = closefrom (fd);
  __analyzer_eval (rc == 0); /* { dg-warning "UNKNOWN" } */
  close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_failed_accept_symbolic_result (int fd)
{
  if (fd < 3)
    return;
  int client = accept (fd, 0, 0);
  if (client >= 0)
    {
      close (client);
      close (fd);
      return;
    }
  int rc = closefrom (fd);
  __analyzer_eval (rc == 0); /* { dg-warning "UNKNOWN" } */
  close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_negative_lowfd_then_close (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (-1);
  close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_symbolic_sign_then_close (const char *path, int lowfd)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  if (lowfd > fd)
    {
      close (fd);
      return;
    }
  closefrom (lowfd);
  close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_errno_cleanup (void)
{
  void *p = malloc (16);
  if (!p)
    return;
  errno = 0;
  int rc = closefrom (-1);
  if (rc < 0 && errno != 0)
    free (p);
  free (p); /* { dg-warning "double-'free' of 'p'" } */
}

void
test_closefrom_equiv_class_order (const char *path, int fd2, const void *addr)
{
  if (bind (fd2, addr, 16) == -1)
    return;
  int fd1 = open (path, O_RDONLY);
  if (fd1 == -1)
    return;
  if (fd1 == fd2)
    {
      closefrom (0);
      close (fd1); /* { dg-warning "double 'close' of file descriptor 'fd1'" } */
    }
  else
    close (fd1);
}
