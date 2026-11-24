/* PR analyzer/106283 */
/* Test closefrom (int) with a void return type (glibc / FreeBSD).  */

extern int accept (int fd, void *addr, unsigned *addrlen);
extern int bind (int fd, const void *addr, unsigned len);
extern int close (int fd);
extern void closefrom (int lowfd);
extern int open (const char *, int mode);
extern int pipe (int fds[2]);
extern int socket (int domain, int type, int protocol);
#define O_RDONLY 0
#define SOCK_STREAM 1
#define AF_INET 2

void
test_closefrom_zero_no_leak (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd >= 0)
    {
      closefrom (0);
      return; /* { dg-bogus "leak of file descriptor" } */
    }
  return; /* { dg-bogus "leak of file descriptor" } */
}

void
test_closefrom_zero_then_close (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd >= 0)
    {
      closefrom (0);
      close (fd); /* { dg-warning "double 'close' of file descriptor 'fd'" } */
    }
}

void
test_closefrom_zero_after_minus_one_check (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd == -1)
    return;
  closefrom (0);
  close (fd); /* { dg-warning "double 'close' of file descriptor 'fd'" } */
}

void
test_closefrom_zero_then_close_pipe_ends (void)
{
  int fds[2];
  if (pipe (fds) != 0)
    return;

  closefrom (0);
  close (fds[0]); /* { dg-warning "double 'close'" } */
  close (fds[1]); /* { dg-warning "double 'close'" } */
}

void
test_closefrom_socket_no_leak (void)
{
  int fd = socket (AF_INET, SOCK_STREAM, 0);
  closefrom (0);
  return; /* { dg-bogus "leak of file descriptor" } */
}

void
test_closefrom_socket_then_close (void)
{
  int fd = socket (AF_INET, SOCK_STREAM, 0);
  if (fd < 0)
    return;
  closefrom (0);
  close (fd); /* { dg-warning "double 'close' of file descriptor 'fd'" } */
}

void
test_closefrom_below_lowfd (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  if (fd < 3)
    {
      closefrom (3);
      return; /* { dg-warning "leak of file descriptor 'fd'" } */
    }
  close (fd);
}

void
test_closefrom_above_lowfd (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  if (fd <= 3)
    {
      close (fd);
      return;
    }
  closefrom (3);
  close (fd); /* { dg-warning "double 'close' of file descriptor 'fd'" } */
}

void
test_closefrom_fds_below_and_at_lowfd (const char *path)
{
  int fd1 = open (path, O_RDONLY);
  if (fd1 < 0)
    return;
  if (fd1 >= 3)
    {
      close (fd1);
      return;
    }

  int fd2 = open (path, O_RDONLY);
  if (fd2 < 0)
    {
      close (fd1);
      return;
    }
  if (fd2 != 3)
    {
      close (fd2);
      close (fd1);
      return;
    }
  closefrom (3);
  close (fd1); /* { dg-bogus "double 'close'" } */
  close (fd2); /* { dg-warning "double 'close' of file descriptor 'fd2'" } */
}

void
test_closefrom_symbolic_in_range (const char *path, int lowfd)
{
  if (lowfd < 0)
    return;
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  if (fd < lowfd)
    {
      close (fd);
      return;
    }
  closefrom (lowfd);
  close (fd); /* { dg-warning "double 'close' of file descriptor 'fd'" } */
}

void
test_closefrom_fixed_lowfd_symbolic_range (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (3);
  if (fd < 3)
    close (fd); /* { dg-bogus "double 'close'" } */
  return; /* { dg-bogus "leak of file descriptor" } */
}

void
test_closefrom_symbolic_lowfd (const char *path, int lowfd)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (lowfd);
  return; /* { dg-bogus "leak of file descriptor" } */
}

void
test_closefrom_symbolic_lowfd_then_close (const char *path, int lowfd)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (lowfd);
  close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_negative_lowfd (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  closefrom (-1);
  return; /* { dg-bogus "leak of file descriptor" } */
}

void
test_closefrom_unchecked_fd_no_leak (const char *path)
{
  int fd = open (path, O_RDONLY);
  closefrom (0);
  return; /* { dg-bogus "leak of file descriptor" } */
}

void
test_closefrom_unchecked_self_as_lowfd (const char *path)
{
  int fd = open (path, O_RDONLY);
  closefrom (fd);
  if (fd == -1)
    close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_unchecked_zero_then_failed_open (const char *path)
{
  int fd = open (path, O_RDONLY);
  closefrom (0);
  if (fd == -1)
    close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_after_failed_bind (int fd)
{
  char addr[16] = {0};
  if (bind (fd, addr, sizeof addr) == 0)
    {
      close (fd);
      return;
    }
  closefrom (0);
  if (fd == -1)
    close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_after_failed_accept (int fd)
{
  int client = accept (fd, 0, 0);
  if (client >= 0)
    {
      close (client);
      close (fd);
      return;
    }
  closefrom (0);
  if (fd == -1)
    close (fd); /* { dg-bogus "double 'close'" } */
}

void
test_closefrom_already_closed_fd (const char *path)
{
  int fd = open (path, O_RDONLY);
  if (fd < 0)
    return;
  close (fd);
  closefrom (0); /* { dg-bogus "double 'close'" } */
  close (fd); /* { dg-warning "double 'close' of file descriptor 'fd'" } */
}
