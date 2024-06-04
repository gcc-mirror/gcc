/* { dg-require-effective-target sockets } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-skip-if "PR analyzer/107750" { *-*-solaris2* } } */

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <errno.h>
#include "analyzer-decls.h"

int test_accept (int fd, struct sockaddr *addr, socklen_t *addrlen)
{
  return accept (fd, addr, addrlen);
}

void test_accept_leak_no_lhs (int fd, struct sockaddr *addr, socklen_t *addrlen)
{
  accept (fd, addr, addrlen); /* { dg-warning "leak of file descriptor" } */
}

void test_accept_leak_with_lhs (int fd, struct sockaddr *addr, socklen_t *addrlen)
{
  int newfd = accept (fd, addr, addrlen); /* { dg-message "socket created here" } */
} /* { dg-warning "leak of file descriptor 'newfd'" } */

int test_accept_null_addr (int fd)
{
  return accept (fd, NULL, 0);
}

int test_accept_uninit_addrlen (int fd)
{
  struct sockaddr_storage addr;
  socklen_t addr_len;
  return accept (fd, (struct sockaddr *)&addr, &addr_len); /* { dg-warning "use of uninitialized value 'addr_len'" } */
}

int test_accept_writes_to_addr_and_len (int fd)
{
  struct sockaddr_storage addr;
  socklen_t addr_len = sizeof (addr);
  __analyzer_eval (addr_len == sizeof (addr)); /* { dg-warning "TRUE" } */
  int newfd = accept (fd, (struct sockaddr *)&addr, &addr_len);
  if (newfd == -1)
    return newfd;
  /* Check that the analyzer considers addr and addr_len to
     have been written to.  */
  __analyzer_eval (((char *)&addr)[0]); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (addr_len == sizeof (addr)); /* { dg-warning "UNKNOWN" } */
  return newfd;
}

void test_accept_on_new_datagram_socket (void)
{
  int fd = socket (AF_UNIX, SOCK_DGRAM, 0);
  if (fd == -1)
    return;
  accept (fd, NULL, NULL); /* { dg-message "'accept' on datagram socket file descriptor 'fd' \\\[-Wanalyzer-fd-type-mismatch\\\]" "warning" } */
  /* { dg-message "'accept' expects a stream socket file descriptor but 'fd' is a datagram socket" "final event" { target *-*-* } .-1 } */
  close (fd);
}

int test_accept_on_accept (int fd_a)
{
  int fd_b = accept (fd_a, NULL, 0);
  if (fd_b == -1)
    return -1;

  int fd_c = accept (fd_b, NULL, 0);  /* { dg-warning "'accept' on file descriptor 'fd_b' in wrong phase \\\[CWE-666\\\] \\\[-Wanalyzer-fd-phase-mismatch\\\]" "warning" } */
  /* { dg-message "'accept' expects a listening stream socket file descriptor but 'fd_b' is connected" "final event" { target *-*-* } .-1 } */

  return fd_b;
}

int test_accept_on_constant ()
{
  return accept (0, NULL, 0);
}
