/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

void test_leak_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "stream socket created here" } */
} /* { dg-warning "leak of file descriptor 'fd'" } */

void test_leak_socket_no_lhs (void)
{
  socket (AF_UNIX, SOCK_STREAM, 0);  /* { dg-warning "leak of file descriptor" } */
}

void test_close_unchecked_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  close (fd);
}

void test_close_checked_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1)
    return;
  close (fd);
}

void test_leak_checked_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "stream socket created here" } */
  if (fd == -1) /* { dg-warning "leak of file descriptor 'fd'" } */
    return;
  // TODO: strange location for leak message
}

void test_bind_on_checked_socket (const char *sockname)
{
  struct sockaddr_un addr;
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1)
    return;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr));
  close (fd);
}

void test_bind_on_unchecked_socket (const char *sockname)
{
  struct sockaddr_un addr;
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "when 'socket' fails" } */
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-warning "'bind' on possibly invalid file descriptor 'fd'" } */
  close (fd);
}

void test_leak_of_bound_socket (const char *sockname)
{
  struct sockaddr_un addr;
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "stream socket created here" } */
  if (fd == -1)
    return;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-warning "leak of file descriptor 'fd'" } */
}

void test_listen_without_bind (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "stream socket created here" } */
  if (fd == -1)
    return;
  listen (fd, 5); /* { dg-warning "'listen' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'listen' expects a bound stream socket file descriptor but 'fd' has not yet been bound" "final event" { target *-*-* } .-1 } */
  close (fd);
}

void test_listen_on_unchecked_bind (const char *sockname)
{
  struct sockaddr_un addr;
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "stream socket created here" } */
  if (fd == -1)
    return;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-message "when 'bind' fails" } */
  listen (fd, 5); /* { dg-warning "'listen' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'listen' expects a bound stream socket file descriptor but 'fd' has not yet been bound" "final event" { target *-*-* } .-1 } */
  close (fd);
}
