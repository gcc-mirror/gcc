/* Various operations done on sockets in the wrong phase.  */

/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

void test_read_on_new_socket (void *buf)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0); /* { dg-message "stream socket created here" } */
  if (fd == -1)
    return;
  read (fd, buf, 1); /* { dg-warning "'read' on file descriptor 'fd' in wrong phase \\\[-Wanalyzer-fd-phase-mismatch\\\]" "warning" } */
  /* { dg-message "'read' expects a stream socket to be connected via 'accept' but 'fd' has not yet been bound" "final event" { target *-*-* } .-1 } */
  close (fd);
}

void test_read_on_bound_socket (int fd, const char *sockname, void *buf)
{
  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  if (bind (fd, (struct sockaddr *)&addr, sizeof (addr)) == -1)
    return;
  /* This could be a datagram socket, so we shouldn't complain here.  */
  read (fd, buf, 1);
}

void test_read_on_listening_socket (int fd, void *buf)
{
  if (listen (fd, 5) == -1) /* { dg-message "stream socket marked as passive here via 'listen'" } */
    return;
  read (fd, buf, 1); /* { dg-message "'read' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'read' expects a stream socket to be connected via the return value of 'accept' but 'fd' is listening; wrong file descriptor\\\?" "final event" { target *-*-* } .-1 } */
}

void test_bind_on_non_socket (const char *filename, const char *sockname)
{
  int fd = open (filename, O_RDONLY);
  if (fd == -1)
    return;

  struct sockaddr_un addr;
  memset (&addr, 0, sizeof (addr));
  addr.sun_family = AF_UNIX;
  strncpy (addr.sun_path, sockname, sizeof(addr.sun_path) - 1);
  int result = bind (fd, (struct sockaddr *)&addr, sizeof (addr)); /* { dg-warning "'bind' on non-socket file descriptor 'fd' \\\[-Wanalyzer-fd-type-mismatch\\\]" "warning" } */
  /* { dg-message "'bind' expects a socket file descriptor but 'fd' is not a socket" "final event" { target *-*-* } .-1 } */
  __analyzer_eval (result == -1); /* { dg-warning "TRUE" } */
  
  close (fd);
}

void test_passive_open_read_on_wrong_socket (int sfd)
{
  int cfd = accept (sfd, NULL, NULL);
  write (sfd, "hello", 6); /* { dg-warning "'write' on file descriptor 'sfd' in wrong phase" "warning" } */
  /* { dg-message "'write' expects a stream socket to be connected via the return value of 'accept' but 'sfd' is listening; wrong file descriptor\\\?" "final event" { target *-*-* } .-1 } */
  close (cfd);
}

void test_listen_on_new_stream_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1)
    return;
  listen (fd, 5); /* { dg-message "'listen' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'listen' expects a bound stream socket file descriptor but 'fd' has not yet been bound" "final event" { target *-*-* } .-1 } */
  close (fd);
}

void test_accept_on_new_stream_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  if (fd == -1)
    return;
  accept (fd, NULL, NULL); /* { dg-message "'accept' on file descriptor 'fd' in wrong phase" "warning" } */
  /* { dg-message "'accept' expects a listening stream socket file descriptor but 'fd' has not yet been bound" "final event" { target *-*-* } .-1 } */
  close (fd);
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
