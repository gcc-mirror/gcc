/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

int test_listen (int fd, int backlog)
{
  return listen (fd, backlog);
}

/* Some systems seem to allow repeated calls to listen.  */

void test_double_listen (int fd, int backlog)
{
  listen (fd, backlog);
  listen (fd, backlog);
}

void test_listen_before_bind (int fd, const char *sockname)
{
  if (listen (fd, 5) == -1) /* { dg-message "stream socket marked as passive here via 'listen'" } */
    return;

  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-warning "'bind' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'bind' expects a new socket file descriptor but 'fd' is already listening" "final event" { target *-*-* } .-1 } */
}

void test_listen_on_unchecked_bind (int fd, const char *sockname)
{
  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  bind (fd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-message "when 'bind' fails" } */
  listen (fd, 5); /* { dg-warning "'listen' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'listen' expects a bound stream socket file descriptor but 'fd' has not yet been bound" "final event" { target *-*-* } .-1 } */
  close (fd);
}

void test_listen_on_new_datagram_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_DGRAM, 0);
  if (fd == -1)
    return;
  listen (fd, 5); /* { dg-message "'listen' on datagram socket file descriptor 'fd' \\\[-Wanalyzer-fd-type-mismatch\\\]" "warning" } */
  /* { dg-message "'listen' expects a stream socket file descriptor but 'fd' is a datagram socket" "final event" { target *-*-* } .-1 } */
  close (fd);
}

void test_listen_on_connected_socket (int fd)
{
  int afd = accept (fd, NULL, 0);
  if (afd == -1)
    return;
  listen (afd, 5); /* { dg-warning "'listen' on file descriptor 'afd' in wrong phase" "warning" } */
  /* { dg-message "'listen' expects a bound stream socket file descriptor but 'afd' is connected" "final event" { target *-*-* } .-1 } */
  close (afd);
}

int test_listen_on_constant ()
{
  return listen (0, 10);
}
